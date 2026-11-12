import {Delaunay} from 'd3-delaunay';
import {polygonArea} from 'd3-polygon';
import {writeFileSync} from 'node:fs';
let seed=29471;
const random=()=>{seed=(Math.imul(seed,1664525)+1013904223)>>>0;return seed/4294967296;};
const datasets=Array.from({length:5},(_,k)=>Array.from({length:20+k*7},()=>[random()*100,random()*100]));
const fixtures=datasets.map(points=>{
  const mesh=Delaunay.from(points),voronoi=mesh.voronoi([0,0,100,100]);
  const edges=[];
  for(let i=0;i<points.length;i++) for(const j of mesh.neighbors(i)) if(i<j) edges.push([i,j]);
  edges.sort((a,b)=>a[0]-b[0]||a[1]-b[1]);
  return {points,edges,areas:points.map((_,i)=>Math.abs(polygonArea(voronoi.cellPolygon(i))))};
});
writeFileSync(new URL('../../tests/fixtures/triangulation.json',import.meta.url),JSON.stringify(fixtures,null,2)+'\n');
