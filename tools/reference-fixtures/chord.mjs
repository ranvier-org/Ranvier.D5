import {chord,chordDirected,chordTranspose} from 'd3-chord';
import {writeFileSync} from 'node:fs';
const matrices=[
  [[1,2,0],[4,0,3],[0,1,2]],
  [[0,5],[0,0]],
  [[7]]
];
const fixtures=[];
for(const [mode,create] of [['Undirected',chord],['Directed',chordDirected],['Transposed',chordTranspose]]) {
  for(const matrix of matrices) {
    for(const sorted of [false,true]) {
      const layout=create().padAngle(0.08);
      if(sorted) layout.sortGroups((a,b)=>b-a).sortSubgroups((a,b)=>b-a).sortChords((a,b)=>b-a);
      const result=layout(matrix);
      fixtures.push({mode,sorted,padAngle:0.08,matrix,groups:result.groups,chords:Array.from(result)});
    }
  }
}
writeFileSync(new URL('../../tests/fixtures/chords.json',import.meta.url),JSON.stringify(fixtures,null,2)+'\n');
