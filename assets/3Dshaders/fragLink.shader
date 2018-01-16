varying vec2 coord;
varying vec3 norm;
uniform sampler2D sampler01;
uniform int use;
uniform float color[4];
void main (void)
{
  gl_FragColor = texture2D(sampler01, coord);
}
