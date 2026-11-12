import {sankey,sankeyLeft,sankeyRight,sankeyCenter,sankeyJustify} from 'd3-sankey';
import {writeFileSync} from 'node:fs';
const cases=[
  {nodes:['a','b','c','d'],links:[['ab','a','b',3],['ac','a','c',2],['bd','b','d',2],['cd','c','d',2]]},
  {nodes:['a','b','c','d','e'],links:[['ab','a','b',4],['bc','b','c',3],['dc','d','c',2]]},
  {nodes:['a','b'],links:[['first','a','b',1],['second','a','b',2]]}
];
const fixtures=[];
for(const input of cases) {
  for(const [alignment,align] of [['Left',sankeyLeft],['Right',sankeyRight],['Center',sankeyCenter],['Justify',sankeyJustify]]) {
    const result=sankey().nodeId(d=>d.id).nodeAlign(align).nodeWidth(20).nodePadding(10).extent([[10,20],[410,260]]).iterations(0)({
      nodes:input.nodes.map(id=>({id})),
      links:input.links.map(([id,source,target,value])=>({id,source,target,value}))
    });
    fixtures.push({alignment,input,
      nodes:result.nodes.map(({id,layer,value,x0,x1,y0,y1})=>({id,layer,value,x0,x1,y0,y1})),
      links:result.links.map(({id,width,y0,y1})=>({id,width,y0,y1}))});
  }
}
writeFileSync(new URL('../../tests/fixtures/sankey.json',import.meta.url),JSON.stringify(fixtures,null,2)+'\n');
