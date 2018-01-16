varying vec3 norm;

void main (void)
{
   gl_FragColor = vec4 ((norm.x+1f)/2f,(norm.y+1f)/2f,(norm.z+1f)/2f,1f);
}
