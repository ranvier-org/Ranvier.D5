import {writeFileSync} from 'node:fs';
import {geoIdentity,geoStream} from 'd3-geo';

const make=config=>geoIdentity().scale(config.scale).translate(config.translate)
  .angle(config.angle).reflectX(config.reflectX).reflectY(config.reflectY);
const configs=[
  {scale:1,translate:[0,0],angle:0,reflectX:false,reflectY:false},
  {scale:2,translate:[100,200],angle:90,reflectX:true,reflectY:false},
  {scale:2.5,translate:[-300,70],angle:37,reflectX:false,reflectY:true},
  {scale:0.001,translate:[1,2],angle:-1234,reflectX:true,reflectY:true}
];
const points=configs.flatMap(config=>[[0,0],[3,4],[123456,-987654]].map(input=>{
  const projection=make(config),output=projection(input);
  return {config,input,output,inverse:projection.invert(output)};
}));
const stream=(projection,rings)=>{
  const output=[];let current;
  geoStream({type:'Polygon',coordinates:rings},projection.stream({
    lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){output.push(current);},
    polygonStart(){},polygonEnd(){},sphere(){throw new Error('No planar sphere');}
  }));
  return output;
};
let state=81;
const random=()=>((state=(Math.imul(state,1664525)+1013904223)>>>0)/4294967296);
const polygons=Array.from({length:40},(_,index)=>{
  const config=configs[index%configs.length],projection=make(config);
  const x=random()*200-100,y=random()*200-100,rx=random()*50+10,ry=random()*50+10;
  const outer=Array.from({length:13},(_,i)=>[x+rx*Math.cos(i*Math.PI/6),y+ry*Math.sin(i*Math.PI/6)]);
  outer[12]=[...outer[0]];
  let rings=[outer];
  if(index%2===0) rings.push(outer.map(([a,b])=>[x+(a-x)/3,y+(b-y)/3]).reverse());
  if(config.reflectX!==config.reflectY) rings=rings.map(ring=>[...ring].reverse());
  const center=projection([x,y]),width=rx*config.scale,height=ry*config.scale;
  const clip=[[center[0]-width/2,center[1]-height/2],[center[0]+width/2,center[1]+height/2]];
  return {config,rings,clip,output:stream(projection.clipExtent(clip),rings)};
});
const outer=[[-20,-20],[20,-20],[20,20],[-20,20],[-20,-20]];
const hole=[[-2,-2],[-2,2],[2,2],[2,-2],[-2,-2]];
const windingCounterexample={
  rings:[outer,hole],reflectX:true,clip:[[-10,-10],[10,10]],
  output:stream(geoIdentity().reflectX(true).clipExtent([[-10,-10],[10,10]]),[outer,hole]),
  expectedFilledArea:384
};
writeFileSync(new URL('../../tests/fixtures/identity.json',import.meta.url),JSON.stringify({schema:1,seed:81,source:{'d3-geo':'3.1.1'},points,polygons,windingCounterexample},null,2)+'\n');
console.log(`Generated ${points.length} identity points and ${polygons.length} polygon cases.`);
