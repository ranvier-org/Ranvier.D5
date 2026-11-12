import {writeFileSync} from 'node:fs';
import {geoEquirectangular,geoMercator,geoOrthographic,geoStream} from 'd3-geo';

const inputs=[
  {name:'hemisphere-exit',kind:'Orthographic',points:[[0,0],[120,0]]},
  {name:'hemisphere-oblique',kind:'Orthographic',points:[[-30,-40],[120,60]]},
  {name:'hemisphere-reentry',kind:'Orthographic',points:[[-20,0],[120,0],[160,20],[20,20]]},
  {name:'rectangle-crossing',kind:'Equirectangular',points:[[-60,0],[60,0]],clip:[[-50,-50],[50,50]]},
  {name:'rectangle-reentry',kind:'Equirectangular',points:[[0,0],[60,0],[60,10],[0,10]],clip:[[-50,-50],[50,50]]},
  {name:'rectangle-oblique',kind:'Equirectangular',points:[[-60,-30],[60,60]],clip:[[-50,-50],[50,50]]}
];
for(const [name,points] of [
  ['meridian',[[0,-89],[0,89]]],
  ['oblique',[[0,-89],[150,80]]],
  ['polar-middle',[[-30,84.9],[30,84.9]]],
  ['polar-seam',[[170,80],[180,89],[-170,80]]],
  ['hidden',[[-20,89],[20,89]]],
  ['pole',[[25,-90],[25,0],[25,90]]]
]) inputs.push({name:'mercator-'+name,kind:'Mercator',points});
const cases=inputs.map(input=>{
  const fragments=[];
  let current;
  const projection=(input.kind==='Orthographic'?geoOrthographic().clipAngle(90):input.kind==='Mercator'?geoMercator():geoEquirectangular())
    .scale(100).translate([0,0]).precision(0.5);
  if(input.clip) projection.clipExtent(input.clip);
  geoStream({type:'LineString',coordinates:input.points},projection.stream({
    lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){fragments.push(current);},
    polygonStart(){throw new Error('Unexpected polygon');},polygonEnd(){throw new Error('Unexpected polygon');},sphere(){throw new Error('Unexpected sphere');}
  }));
  return {...input,scale:100,precision:0.5,tolerance:input.clip||input.kind==='Mercator'?1.001:1e-9,fragments};
});
const output={schema:1,source:{'d3-geo':'3.1.1'},coordinates:'Input degrees, screen Y down. Rectangle intersections allow the combined adaptive screen-error tolerance; analytic hemisphere intersections use 1e-9.',cases};
writeFileSync(new URL('../../tests/fixtures/clipped-streams.json',import.meta.url),JSON.stringify(output,null,2)+'\n');
console.log(`Generated ${cases.length} clipped stream cases.`);
