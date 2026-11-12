import {writeFileSync} from 'node:fs';
import {geoAlbersUsa,geoAzimuthalEqualArea,geoConicEqualArea,geoEquirectangular,geoMercator,geoOrthographic,geoPath,geoStream,geoTransverseMercator} from 'd3-geo';

// Fit cases pin center [0,0] and precision 0.1 so D3 and D5 measure the same projected stream.
const kinds=[
  ['Equirectangular',()=>geoEquirectangular(),null],
  ['Mercator',()=>geoMercator(),null],
  ['Orthographic',()=>geoOrthographic(),null],
  ['AzimuthalEqualArea',()=>geoAzimuthalEqualArea(),null],
  ['ConicEqualArea',()=>geoConicEqualArea().parallels([29.5,45.5]),[29.5,45.5]],
  ['TransverseMercator',()=>geoTransverseMercator(),null]
];
const ring=(lon,lat,rx,ry)=>{
  const points=Array.from({length:13},(_,i)=>{const angle=-i*Math.PI/6;return [lon+rx*Math.cos(angle),lat+ry*Math.sin(angle)];});
  points[12]=[...points[0]];
  return points;
};
const geometries=[
  {name:'polygon',geometry:{type:'Polygon',coordinates:[ring(10,20,25,15)]}},
  {name:'line',geometry:{type:'LineString',coordinates:[[-40,-10],[30,35],[60,5]]}},
  {name:'points',geometry:{type:'MultiPoint',coordinates:[[-20,-15],[35,40],[5,10]]}},
  {name:'sphere',geometry:{type:'Sphere'}}
];
const fits=[
  {mode:'extent',extent:[[20,30],[620,430]]},
  {mode:'size',size:[960,500]},
  {mode:'width',width:400},
  {mode:'height',height:300}
];
const cases=[];
for(const [kind,create,parallels] of kinds){
  for(const {name,geometry} of geometries){
    for(const fit of fits){
      const projection=create().center([0,0]).precision(0.1).rotate([15,-10,0]);
      if(fit.mode==='extent') projection.fitExtent(fit.extent,geometry);
      else if(fit.mode==='size') projection.fitSize(fit.size,geometry);
      else if(fit.mode==='width') projection.fitWidth(fit.width,geometry);
      else projection.fitHeight(fit.height,geometry);
      cases.push({kind,parallels,name,geometry,...fit,rotate:[15,-10,0],precision:0.1,scale:projection.scale(),translate:projection.translate(),bounds:geoPath(projection).bounds(geometry)});
    }
  }
}
writeFileSync(new URL('../../tests/fixtures/projection-fit.json',import.meta.url),JSON.stringify({schema:1,source:{'d3-geo':'3.1.1'},
  comparison:'Fitted scale and translation relative to the fitted extent; tolerance scales with the combined adaptive precision.',cases},null,2)+'\n');

const usa=geoAlbersUsa();
const places=[[-122.42,37.77],[-74.01,40.71],[-87.63,41.88],[-149.9,61.22],[-157.86,21.31],[-155.08,19.72],[-80.19,25.76],[-68.0,46.0],[-170.0,60.0],[0,0],[-100,30]];
const projected=places.map(point=>{const value=usa(point);return {point,projected:value,inverse:value?usa.invert(value):null};});
const state=(lon,lat,dx,dy)=>({type:'Polygon',coordinates:[[[lon-dx,lat-dy],[lon-dx,lat+dy],[lon+dx,lat+dy],[lon+dx,lat-dy],[lon-dx,lat-dy]]]});
const shapes=[
  {name:'colorado',geometry:state(-105.5,39,3.5,2)},
  {name:'alaska-interior',geometry:state(-150,64,8,3)},
  {name:'hawaii-big-island',geometry:state(-155.5,19.6,0.6,0.6)},
  {name:'lower48-band',geometry:state(-98,38,22,8)}
].map(({name,geometry})=>{
  const output=[];let current;
  geoStream(geometry,usa.precision(0.1).stream({
    lineStart(){current=[];},point(x,y){current.push([x,y]);},lineEnd(){output.push(current);},
    polygonStart(){},polygonEnd(){},sphere(){}
  }));
  return {name,geometry,output};
});
writeFileSync(new URL('../../tests/fixtures/albers-usa.json',import.meta.url),JSON.stringify({schema:1,source:{'d3-geo':'3.1.1'},
  coordinates:'Default D3 Albers USA: scale 1070, translate [480,250]. Points outside every inset clip are null.',points:projected,shapes,precision:0.1},null,2)+'\n');
console.log(`Generated ${cases.length} fit cases, ${projected.length} Albers USA points and ${shapes.length} shapes.`);
