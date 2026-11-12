import {writeFileSync} from 'node:fs';
import {geoEquirectangular,geoMercator,geoOrthographic,geoRotation} from 'd3-geo';

const points=[[-170,-60],[-90,-30],[-45,70],[0,0],[30,55],[90,0],[170,40]];
const configurations=[
  {scale:1,translate:[0,0],center:[0,0],rotate:[0,0,0]},
  {scale:135,translate:[360,240],center:[10,5],rotate:[20,-15,10]}
];
const cases=[];
for(const [kind,create] of [['Equirectangular',geoEquirectangular],['Mercator',geoMercator],['Orthographic',geoOrthographic]]) {
  for(const config of configurations) {
    const projection=create().scale(config.scale).translate(config.translate).center(config.center).rotate(config.rotate);
    const rotate=geoRotation(config.rotate);
    for(const point of points) {
      const rotated=rotate(point);
      // D3's point function bypasses stream clipping; record visibility separately.
      const visible=kind!=='Orthographic' || Math.cos(rotated[0]*Math.PI/180)*Math.cos(rotated[1]*Math.PI/180)>=0;
      cases.push({kind,...config,point,visible,projected:projection(point)});
    }
  }
}
const output={schema:1,seed:42,source:{'d3-geo':'3.1.1'},coordinates:'Input and rotation degrees; scale in screen units per raw projected unit; screen Y down. Orthographic visibility recorded separately from D3 point projection.',tolerance:1e-9,cases};
writeFileSync(new URL('../../tests/fixtures/projections.json',import.meta.url),JSON.stringify(output,null,2)+'\n');
console.log(`Generated ${cases.length} projection cases.`);
