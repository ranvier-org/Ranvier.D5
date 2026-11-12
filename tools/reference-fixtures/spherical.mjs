import {writeFileSync} from 'node:fs';
import {geoArea,geoBounds,geoCentroid,geoDistance,geoInterpolate,geoLength,geoRotation} from 'd3-geo';

const polygon = rings => ({type:'Polygon',coordinates:rings});
const octant=[[0,0],[0,90],[90,0],[0,0]];
const small=[[0,0],[0,20],[20,20],[20,0],[0,0]];
const hole=[[5,5],[15,5],[15,15],[5,15],[5,5]];
const geometries=[
  ['sphere',{type:'Sphere'}],
  ['empty-collection',{type:'GeometryCollection',geometries:[]}],
  ['point',{type:'Point',coordinates:[135,-32]}],
  ['coincident',{type:'MultiPoint',coordinates:[[5,10],[5,10],[5,10]]}],
  ['equator-quarter',{type:'LineString',coordinates:[[0,0],[90,0]]}],
  ['north-arc-extremum',{type:'LineString',coordinates:[[-45,60],[45,60]]}],
  ['dateline-line',{type:'LineString',coordinates:[[170,10],[-170,10]]}],
  ['octant',polygon([octant])],
  ['octant-complement',polygon([[...octant].reverse()])],
  ['small-square',polygon([small])],
  ['square-with-hole',polygon([small,hole])],
  ['dateline-polygon',polygon([[[170,-10],[170,10],[-170,10],[-170,-10],[170,-10]]])],
  ['north-cap',polygon([[[-120,80],[120,80],[0,80],[-120,80]]])],
  ['north-cap-complement',polygon([[[-120,80],[0,80],[120,80],[-120,80]]])],
  ['multipolygon',{type:'MultiPolygon',coordinates:[[small],[[[30,0],[30,10],[40,10],[40,0],[30,0]]]]}],
  ['line-and-point',{type:'GeometryCollection',geometries:[{type:'LineString',coordinates:[[0,0],[90,0]]},{type:'Point',coordinates:[-45,70]}]}]
];
const pairs=[
  ['same',[10,20],[10,20]],
  ['quarter',[0,0],[90,0]],
  ['dateline',[170,10],[-170,10]],
  ['near-antipodes',[0,0],[179.999,0.001]],
  ['near-pole',[0,89.999],[90,89.999]],
  ['tiny',[0,0],[1e-9,1e-9]],
  ['oblique',[-73.9857,40.7484],[139.6917,35.6895]]
];
const rotations=[
  {angles:[0,0,0],point:[120,-45]},
  {angles:[45,0,0],point:[170,20]},
  {angles:[30,40,50],point:[-125,60]},
  {angles:[-180,-90,0],point:[0,0]}
].map(({angles,point})=>{const rotate=geoRotation(angles);return {angles,point,rotated:rotate(point),inverse:rotate.invert(rotate(point))};});
const output={schema:1,seed:42,source:{'d3-geo':'3.1.1'},
  coordinates:'Public longitude/latitude degrees; distance and length radians; area steradians; D3 clockwise small exterior spherical rings; NaN from empty or ambiguous reference results serialized as null.',
  tolerance:1e-9,
  independentExpectations:[{
    name:'north-cap',bounds:[[-180,80],[180,90]],
    reason:'The three shortest great-circle arcs joining latitude-80 vertices enclose the north pole and stay north of latitude 80. d3-geo 3.1.1 instead returns full-sphere bounds because bounds.polygonEnd tests the unnormalized areaRingSum sign. Preserve the raw reference result separately; do not copy that overly conservative latitude bound.'
  }],
  geometries:geometries.map(([name,geometry])=>({name,geometry,area:geoArea(geometry),length:geoLength(geometry),centroid:geoCentroid(geometry),bounds:geoBounds(geometry)})),
  pairs:pairs.map(([name,from,to])=>({name,from,to,distance:geoDistance(from,to),samples:[0,.25,.5,.75,1].map(t=>({t,point:geoInterpolate(from,to)(t)}))})),rotations};
writeFileSync(new URL('../../tests/fixtures/spherical.json',import.meta.url),JSON.stringify(output,null,2)+'\n');
console.log(`Generated ${output.geometries.length} geometry, ${output.pairs.length} geodesic and ${rotations.length} rotation cases.`);
