varying vec2 coord;
varying vec3 norm;
uniform sampler2D sampler01;

uniform float scalex;
uniform float scaley;

void main (void)
{
   vec2 scale = vec2(scalex * coord.x,scaley * coord.y);
   gl_FragColor = texture2D(sampler01, fract(scale));
}
