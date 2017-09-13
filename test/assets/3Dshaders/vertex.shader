attribute vec3 position;
attribute vec2 texCoord;
attribute vec3 normal;

float factor;
varying vec2 coord;
varying vec3 norm;
void main(void)
{
   vec4 a = gl_Vertex;
   factor = 1;
   a.x = a.x * factor;
   a.y = a.y * factor;


   coord = texCoord;
   norm = normal;
   gl_Position = gl_ModelViewProjectionMatrix * a;
}   