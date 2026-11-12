import {orient2d,incircle} from 'robust-predicates';
import {polygonArea,polygonCentroid} from 'd3-polygon';
import {writeFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
let seed=42;
const random=()=>((seed=(Math.imul(seed,1664525)+1013904223)>>>0)/4294967296*200-100);
const cases=Array.from({length:100},()=>Array.from({length:4},()=>[random(),random()]));
cases.push([[0,0],[1,0],[0,1],[1,1]],[[0,0],[1,1],[2,2],[3,3]],[[0,0],[1,1],[2,2+Number.EPSILON*2],[1,0]]);
const polygons=[[[0,0],[2,0],[2,2],[0,2]],[[0,0],[3,0],[1,2]],[[0,0],[3,0],[3,3],[2,1],[0,3]]];
writeFileSync(fileURLToPath(new URL('../../tests/fixtures/planar.json',import.meta.url)),JSON.stringify({
  convention:'Cartesian counterclockwise positive orientation/area; incircle positive inside CCW circle',
  predicates:cases.map(points=>({points,orientation:-Math.sign(orient2d(...points.slice(0,3).flat())),incircle:Math.sign(incircle(...points.flat()))})),
  polygons:polygons.map(points=>({points,area:-polygonArea(points),centroid:polygonCentroid(points)}))
},null,2)+'\n');
