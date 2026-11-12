import {stratify, tree, cluster, partition} from "d3-hierarchy";
import {writeFileSync, copyFileSync} from "node:fs";
const rows=[["r",null],["a","r"],["b","r"],["c","a"],["d","a"],["e","b"],["f","e"],["g","e"]];
const build=()=>stratify().id(d=>d[0]).parentId(d=>d[1])(rows).sum(d=>rows.some(x=>x[1]===d[0])?0:1);
const positions=layout=>layout(build()).descendants().map(n=>({id:n.id,x:n.x,y:n.y})).sort((a,b)=>a.id.localeCompare(b.id));
writeFileSync("../../tests/fixtures/hierarchy.json",JSON.stringify({rows,tree:positions(tree().size([400,300])),cluster:positions(cluster().size([400,300])),partition:partition().size([400,300])(build()).descendants().map(n=>({id:n.id,x0:n.x0,y0:n.y0,x1:n.x1,y1:n.y1}))},null,2)+"\n");
copyFileSync("node_modules/d3-hierarchy/LICENSE","licenses/d3-hierarchy.txt");
