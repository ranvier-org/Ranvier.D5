import {writeFileSync} from 'node:fs';
import {geoConicConformal,geoConicEqualArea,geoConicEquidistant,geoPath,geoRotation,geoStream,geoTransverseMercator} from 'd3-geo';

const kinds=[
  {kind:'ConicConformal',parallels:[30,30],create:geoConicConformal},
  {kind:'ConicConformal',parallels:[20,50],create:geoConicConformal},
  {kind:'ConicConformal',parallels:[-10,-40],create:geoConicConformal},
  {kind:'ConicEqualArea',parallels:[0,60],create:geoConicEqualArea},
  {kind:'ConicEqualArea',parallels:[29.5,45.5],create:geoConicEqualArea},
  {kind:'ConicEquidistant',parallels:[0,60],create:geoConicEquidistant},
  {kind:'ConicEquidistant',parallels:[-20,30],create:geoConicEquidistant},
  {kind:'TransverseMercator',parallels:null,create:geoTransverseMercator}
];
const configure=(entry,config)=>{
  const projection=entry.create().scale(config.scale).translate(config.translate).center(config.center).rotate(config.rotate);
  if(entry.parallels) projection.parallels(entry.parallels);
  return projection;
};
const points=[[-170,-60],[-90,-30],[-45,70],[0,0],[30,55],[90,0],[170,40],[0,89.5],[135,-10],[179.5,-5]];
const configurations=[
  {scale:1,translate:[0,0],center:[0,0],rotate:[0,0,0]},
  {scale:135,translate:[360,240],center:[10,5],rotate:[20,-15,10]}
];
const pointCases=[];
for(const entry of kinds){
  for(const config of configurations){
    const projection=configure(entry,config);
    // Transverse Mercator rolls 90 degrees; a point landing exactly on a rotated pole is singular in D5's exact trigonometry.
    const rolled=geoRotation([config.rotate[0],config.rotate[1],config.rotate[2]+90]);
    for(const point of points){
      const defined=entry.kind!=='TransverseMercator' || Math.abs(rolled(point)[1])<90-1e-9;
      const projected=defined?projection(point):null;
      pointCases.push({kind:entry.kind,parallels:entry.parallels,...config,point,defined,projected,inverse:defined?projection.invert(projected):null});
    }
  }
}
writeFileSync(new URL('../../tests/fixtures/conic-projections.json',import.meta.url),JSON.stringify({schema:1,seed:42,source:{'d3-geo':'3.1.1'},
  coordinates:'Input, parallels and rotation degrees; scale in screen units per raw projected unit; screen Y down. Transverse Mercator applies the D3 extra 90 degree roll and swapped center.',
  tolerance:1e-9,cases:pointCases},null,2)+'\n');

const small=[[-10,-10],[-10,10],[10,10],[10,-10],[-10,-10]];
const hole=[[-3,-3],[3,-3],[3,3],[-3,3],[-3,-3]];
const seam=[[170,-20],[170,20],[-170,20],[-170,-20],[170,-20]];
const seamHole=[[175,-5],[-175,-5],[-175,5],[175,5],[175,-5]];
const northCap=[[180,70],[90,70],[0,70],[-90,70],[180,70]];
const southCap=[[180,-70],[-90,-70],[0,-70],[90,-70],[180,-70]];
const polygons=[
  {name:'small',rings:[small]},
  {name:'hole',rings:[small,hole]},
  {name:'complement',rings:[[...small].reverse()]},
  {name:'seam',rings:[seam]},
  {name:'seam-hole',rings:[seam,seamHole]},
  {name:'north-cap',rings:[northCap]},
  {name:'south-cap',rings:[southCap]},
  {name:'north-complement',rings:[[...northCap].reverse()]}
];
let state=42;
const random=()=>((state=(Math.imul(state,1664525)+1013904223)>>>0)/4294967296);
for(let index=0;index<8;index++){
  const lon=random()*340-170,lat=random()*120-60;
  const rx=5+random()*25,ry=5+random()*15;
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
  {name:'seam',points:[[160,10],[-160,20]]},
  {name:'meridian',points:[[30,-80],[30,80]]},
  {name:'polar',points:[[-60,60],[120,60]]},
  {name:'parallel',points:[[-150,40],[-50,40],[50,40],[150,40]]}
];
// Tests compare topology, oriented area and perimeter for polygons and endpoints for lines, so store only those.
const ringArea=ring=>ring.reduce((sum,[x,y],i)=>{const [u,v]=ring[(i+1)%ring.length];return sum+x*v-u*y;},0)/2;
const ringPerimeter=ring=>ring.reduce((sum,[x,y],i)=>{const [u,v]=ring[(i+1)%ring.length];return sum+Math.hypot(u-x,v-y);},0);
const summarize=(collect,output)=>collect==='output'
  ?{ringCount:output.length,area:output.reduce((sum,ring)=>sum+ringArea(ring),0),perimeter:output.reduce((sum,ring)=>sum+ringPerimeter(ring),0)}
  :{fragments:output.map(fragment=>[fragment[0],fragment[fragment.length-1]])};
const streamCases=[];
for(const entry of kinds){
  const record=(input,geometry,collect)=>{
    const output=[];let current;
    // D3 conic constructors carry non-zero default centers; pin D5's default origin.
    const projection=entry.create().scale(100).translate([0,0]).center([0,0]).precision(0.1).rotate(input.rotate??[0,0,0]);
    if(entry.parallels) projection.parallels(entry.parallels);
    // D3 Mercator-family projections clip to their world square; record its extent for comparison.
    const extent=projection.clipExtent?.();
    geoStream(geometry,projection.stream({
      lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){output.push(current);},
      polygonStart(){},polygonEnd(){},sphere(){throw new Error('Unexpected sphere');}
    }));
    streamCases.push({...input,kind:entry.kind,parallels:entry.parallels,scale:100,precision:0.1,extent,...summarize(collect,output)});
  };
  for(const input of polygons) record(input,{type:'Polygon',coordinates:input.rings},'output');
  for(const input of lines) record({...input,line:true},{type:'LineString',coordinates:input.points},'fragments');
}
const spheres=kinds.map(entry=>{
  const projection=entry.create().scale(100).translate([0,0]).center([0,0]).precision(0.1);
  if(entry.parallels) projection.parallels(entry.parallels);
  const path=geoPath(projection);
  return {kind:entry.kind,parallels:entry.parallels,scale:100,precision:0.1,area:path.area({type:'Sphere'}),perimeter:path.measure({type:'Sphere'})};
});
writeFileSync(new URL('../../tests/fixtures/conic-streams.json',import.meta.url),JSON.stringify({schema:1,seed:42,source:{'d3-geo':'3.1.1'},
  comparison:'Polygons: ring count, summed oriented area and perimeter of the D3 output (area tolerance is perimeter times combined precision). Lines: first and last point of each fragment. Spheres: projected world area within perimeter times combined precision.',
  cases:streamCases,spheres},null,2)+'\n');
console.log(`Generated ${pointCases.length} conic point cases and ${streamCases.length} conic stream cases.`);
