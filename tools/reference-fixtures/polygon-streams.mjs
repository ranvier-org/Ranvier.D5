import {writeFileSync} from 'node:fs';
import {geoEquirectangular,geoMercator,geoOrthographic,geoStream} from 'd3-geo';

const small=[[-10,-10],[-10,10],[10,10],[10,-10],[-10,-10]];
const hole=[[-3,-3],[3,-3],[3,3],[-3,3],[-3,-3]];
const seam=[[170,-20],[170,20],[-170,20],[-170,-20],[170,-20]];
const seamHole=[[175,-5],[-175,-5],[-175,5],[175,5],[175,-5]];
const back=[[-120,-40],[-120,40],[120,40],[120,-40],[-120,-40]];
const inputs=[
  {name:'small',kind:'Equirectangular',rings:[small]},
  {name:'hole',kind:'Equirectangular',rings:[small,hole]},
  {name:'complement',kind:'Equirectangular',rings:[[...small].reverse()]},
  {name:'seam',kind:'Equirectangular',rings:[seam]},
  {name:'seam-hole',kind:'Equirectangular',rings:[seam,seamHole]},
  {name:'globe-small',kind:'Orthographic',rings:[small,hole]},
  {name:'globe-cover',kind:'Orthographic',rings:[[...back].reverse()]},
  {name:'globe-hidden',kind:'Orthographic',rings:[back]},
  {name:'globe-crossing',kind:'Orthographic',rings:[[[-40,-35],[-40,45],[130,45],[130,-35],[-40,-35]]]}
];
inputs.push({name:'seam-touching-hole',kind:'Equirectangular',rings:[
  [[160,0],[180,20],[-160,0],[180,-20],[160,0]],
  [[180,20],[175,0],[-175,0],[180,20]]
]});
let state=42;
const random=()=>((state=(Math.imul(state,1664525)+1013904223)>>>0)/4294967296);
for(let index=0;index<40;index++){
  const lon=random()*340-170,lat=random()*120-60;
  const rx=5+random()*25,ry=5+random()*15;
  const ring=Array.from({length:13},(_,i)=>{
    const angle=-i*Math.PI/6;
    return [lon+rx*Math.cos(angle),lat+ry*Math.sin(angle)];
  });
  ring[12]=[...ring[0]];
  const rings=[index%3===0?[...ring].reverse():ring];
  if(index%3===1){
    const inner=ring.map(([x,y])=>[lon+(x-lon)/3,lat+(y-lat)/3]).reverse();
    rings.push(inner);
  }
  inputs.push({name:`seeded-${index}`,kind:index%2?'Orthographic':'Equirectangular',rings,rotate:[random()*360-180,random()*100-50,random()*60-30]});
}
for(const input of [...inputs]){
  const projection=(input.kind==='Orthographic'?geoOrthographic():geoEquirectangular())
    .scale(100).translate([0,0]).rotate(input.rotate??[0,0,0]);
  const [x,y]=projection(input.rings[0][0]);
  const width=20+random()*80,height=20+random()*80;
  inputs.push({...input,name:input.name+'-rectangle',clip:[[x-width/2,y-height/2],[x+width/2,y+height/2]]});
}
for(const input of inputs.filter(input=>!input.clip)){
  inputs.push({...input,name:'mercator-'+input.name,kind:'Mercator'});
}
for(const latitude of [-89,-85,85,89]){
  const ring=[[-30,latitude-0.5],[-30,latitude+0.5],[30,latitude+0.5],[30,latitude-0.5],[-30,latitude-0.5]];
  inputs.push({name:'mercator-polar-'+latitude,kind:'Mercator',rings:[ring]});
  inputs.push({name:'mercator-polar-complement-'+latitude,kind:'Mercator',rings:[[...ring].reverse()]});
}
const cases=inputs.map(input=>{
  const output=[]; let current;
  const projection=(input.kind==='Orthographic'?geoOrthographic().clipAngle(90):input.kind==='Mercator'?geoMercator():geoEquirectangular())
    .scale(100).translate([0,0]).precision(0.1).rotate(input.rotate??[0,0,0]);
  if(input.clip) projection.clipExtent(input.clip);
  geoStream({type:'Polygon',coordinates:input.rings},projection.stream({
    lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){output.push(current);},
    polygonStart(){},polygonEnd(){},sphere(){throw new Error('Unexpected sphere');}
  }));
  return {...input,scale:100,precision:0.1,output};
});
writeFileSync(new URL('../../tests/fixtures/polygon-streams.json',import.meta.url),JSON.stringify({schema:1,seed:42,source:{'d3-geo':'3.1.1'},comparison:'Ring topology and oriented filled area; area tolerance is perimeter times combined screen precision plus pi times squared combined precision.',cases},null,2)+'\n');
console.log(`Generated ${cases.length} polygon stream cases.`);
