import {writeFileSync} from 'node:fs';
import {geoAzimuthalEqualArea,geoAzimuthalEquidistant,geoGnomonic,geoOrthographic,geoRotation,geoStereographic,geoStream} from 'd3-geo';

// D3 default clip angles; null keeps the projection's own default.
const kinds=[
  ['AzimuthalEqualArea',geoAzimuthalEqualArea,180-1e-3],
  ['AzimuthalEquidistant',geoAzimuthalEquidistant,180-1e-3],
  ['Gnomonic',geoGnomonic,60],
  ['Stereographic',geoStereographic,142],
  ['Orthographic',geoOrthographic,90]
];
const radians=Math.PI/180;
const points=[[-170,-60],[-90,-30],[-45,70],[0,0],[30,55],[90,0],[170,40],[0,89.5],[135,-10]];
const configurations=[
  {scale:1,translate:[0,0],center:[0,0],rotate:[0,0,0]},
  {scale:135,translate:[360,240],center:[10,5],rotate:[20,-15,10]}
];
const pointCases=[];
for(const [kind,create] of kinds){
  for(const config of configurations){
    const projection=create().scale(config.scale).translate(config.translate).center(config.center).rotate(config.rotate);
    const rotate=geoRotation(config.rotate);
    for(const point of points){
      const [lambda,phi]=rotate(point).map(value=>value*radians);
      const cosine=Math.cos(lambda)*Math.cos(phi);
      // D3 point projection does not clip; D5 omits hidden or non-injective positions.
      // Radian round-off leaves cos(90 degrees) near 6e-17; D5 evaluates cardinal angles exactly, so the horizon itself is undefined.
      const defined=kind==='Orthographic'?cosine>=-1e-12:kind==='Gnomonic'?cosine>1e-12:cosine>-1+1e-12;
      const projected=defined?projection(point):null;
      const inverse=defined?projection.invert(projected):null;
      pointCases.push({kind,...config,point,defined,projected,inverse});
    }
  }
}
writeFileSync(new URL('../../tests/fixtures/azimuthal-projections.json',import.meta.url),JSON.stringify({schema:1,seed:42,source:{'d3-geo':'3.1.1'},
  coordinates:'Input and rotation degrees; scale in screen units per raw projected unit; screen Y down. Positions behind the orthographic limb, gnomonic horizon or exact antipode are recorded as undefined.',
  tolerance:1e-9,cases:pointCases},null,2)+'\n');

const small=[[-10,-10],[-10,10],[10,10],[10,-10],[-10,-10]];
const hole=[[-3,-3],[3,-3],[3,3],[-3,3],[-3,-3]];
const wide=[[-75,-35],[-75,45],[75,45],[75,-35],[-75,-35]];
const back=[[100,-40],[100,40],[-100,40],[-100,-40],[100,-40]];
const polygons=[
  {name:'small',rings:[small]},
  {name:'hole',rings:[small,hole]},
  {name:'complement',rings:[[...small].reverse()]},
  {name:'wide',rings:[wide]},
  {name:'wide-hole',rings:[wide,hole]},
  {name:'behind',rings:[back]},
  {name:'behind-complement',rings:[[...back].reverse()]}
];
let state=42;
const random=()=>((state=(Math.imul(state,1664525)+1013904223)>>>0)/4294967296);
for(let index=0;index<12;index++){
  const lon=random()*340-170,lat=random()*120-60;
  const rx=10+random()*40,ry=10+random()*30;
  const ring=Array.from({length:13},(_,i)=>{
    const angle=-i*Math.PI/6;
    return [lon+rx*Math.cos(angle),lat+ry*Math.sin(angle)];
  });
  ring[12]=[...ring[0]];
  const rings=[index%3===0?[...ring].reverse():ring];
  if(index%3===1) rings.push(ring.map(([x,y])=>[lon+(x-lon)/3,lat+(y-lat)/3]).reverse());
  polygons.push({name:`seeded-${index}`,rings,rotate:[random()*360-180,random()*100-50,random()*60-30]});
}
const lines=[
  {name:'through-center',points:[[-170,0],[0,0],[170,0]]},
  {name:'oblique',points:[[-80,-50],[60,70]]},
  {name:'reentry',points:[[-20,0],[120,0],[160,20],[20,20]]},
  {name:'outside-dip',points:[[-75,10],[75,10]]},
  {name:'pole',points:[[25,-90],[25,0],[25,90]]}
];
// Tests compare topology, oriented area and perimeter for polygons and endpoints for lines, so store only those.
const ringArea=ring=>ring.reduce((sum,[x,y],i)=>{const [u,v]=ring[(i+1)%ring.length];return sum+x*v-u*y;},0)/2;
const ringPerimeter=ring=>ring.reduce((sum,[x,y],i)=>{const [u,v]=ring[(i+1)%ring.length];return sum+Math.hypot(u-x,v-y);},0);
const summarize=(collect,output)=>collect==='output'
  ?{ringCount:output.length,area:output.reduce((sum,ring)=>sum+ringArea(ring),0),perimeter:output.reduce((sum,ring)=>sum+ringPerimeter(ring),0)}
  :{fragments:output.map(fragment=>[fragment[0],fragment[fragment.length-1]])};
const streamCases=[];
const record=(kind,create,clipAngle,input,geometry,collect)=>{
  const output=[];let current;
  const projection=create().scale(100).translate([0,0]).precision(0.1).rotate(input.rotate??[0,0,0]).clipAngle(clipAngle);
  geoStream(geometry,projection.stream({
    lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){output.push(current);},
    polygonStart(){},polygonEnd(){},sphere(){throw new Error('Unexpected sphere');}
  }));
  streamCases.push({...input,kind,clipAngle,scale:100,precision:0.1,...summarize(collect,output)});
};
for(const [kind,create,clipAngle] of kinds){
  for(const input of polygons) record(kind,create,clipAngle,input,{type:'Polygon',coordinates:input.rings},'output');
  for(const input of lines) record(kind,create,clipAngle,{...input,line:true},{type:'LineString',coordinates:input.points},'fragments');
}
for(const input of polygons.slice(0,7)) record('Gnomonic',geoGnomonic,45,{...input,name:input.name+'-narrow'},{type:'Polygon',coordinates:input.rings},'output');
for(const input of lines) record('Stereographic',geoStereographic,170,{...input,name:input.name+'-wide',line:true},{type:'LineString',coordinates:input.points},'fragments');
writeFileSync(new URL('../../tests/fixtures/azimuthal-streams.json',import.meta.url),JSON.stringify({schema:1,seed:42,source:{'d3-geo':'3.1.1'},
  comparison:'Polygons: ring count, summed oriented area and perimeter of the D3 output (area tolerance is perimeter times combined precision). Lines: first and last point of each fragment.',
  cases:streamCases},null,2)+'\n');
console.log(`Generated ${pointCases.length} azimuthal point cases and ${streamCases.length} azimuthal stream cases.`);
