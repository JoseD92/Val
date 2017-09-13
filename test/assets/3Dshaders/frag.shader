varying vec2 coord;
varying vec3 norm;
uniform sampler2D sampler01;
uniform int use;
uniform float color[4];
void main (void)
{
   if (use==0) gl_FragColor = vec4 (coord.x,coord.y,0,1);
   if (use==1) gl_FragColor = texture2D(sampler01, coord);
   if (use==2) gl_FragColor = vec4 ((norm.x+1)/2,(norm.y+1)/2,(norm.z+1)/2,1);
   if (use==4) gl_FragColor = vec4 (color[0],color[1],color[2],color[3]);
}
