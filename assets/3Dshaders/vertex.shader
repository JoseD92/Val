attribute vec3 position;
attribute vec2 texCoord;
attribute vec3 normal;

float factor;
varying vec2 coord;
varying vec3 norm;
void main(void)
{
   coord = texCoord;
   norm = normal;
   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}   
