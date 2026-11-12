import {writeFileSync} from 'node:fs';
import {geoEquirectangular,geoMercator,geoStream} from 'd3-geo';

const inputs=[
  {name:'eastward',kind:'Equirectangular',points:[[170,10],[-170,10]],rotate:[0,0,0]},
  {name:'westward',kind:'Equirectangular',points:[[-170,10],[170,10]],rotate:[0,0,0]},
  {name:'oblique-high-latitude',kind:'Equirectangular',points:[[150,65],[-140,75]],rotate:[0,0,0]},
  {name:'mercator-oblique',kind:'Mercator',points:[[165,-35],[-145,20]],rotate:[0,0,0]},
  {name:'rotated',kind:'Equirectangular',points:[[-10,10],[10,10]],rotate:[180,0,0]}
];
const cases=inputs.map(input=>{
  const fragments=[];
  let current;
  const projection=(input.kind==='Mercator'?geoMercator():geoEquirectangular())
    .scale(100).translate([0,0]).precision(0.5).rotate(input.rotate);
  geoStream({type:'LineString',coordinates:input.points},projection.stream({
    lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){fragments.push(current);},
    polygonStart(){throw new Error('Unexpected polygon');},polygonEnd(){throw new Error('Unexpected polygon');},sphere(){throw new Error('Unexpected sphere');}
  }));
  return {...input,scale:100,precision:0.5,fragments};
});
const output={schema:1,seed:42,source:{'d3-geo':'3.1.1'},coordinates:'Input and rotation degrees; projected screen coordinates with Y down. Compare fragment topology and endpoints, not identical adaptive subdivision vertices.',tolerance:1e-9,cases};
writeFileSync(new URL('../../tests/fixtures/seam-streams.json',import.meta.url),JSON.stringify(output,null,2)+'\n');
console.log(`Generated ${cases.length} seam stream cases.`);
