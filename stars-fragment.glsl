#version 410

// from https://www.shadertoy.com/view/XlfGRj

uniform vec3 resolution; // viewport resolution (in pixels)
// uniform float time;      // shader playback time (in seconds)
// uniform vec4 mouse;      // mouse pixel coords. xy: current (if MLB down), zw: click
uniform vec2 origin;


// Star Nest by Pablo Roman Andrioli

// This content is under the MIT License.

#define iterations 17
#define formuparam 0.53

#define volsteps 8
#define stepsize 0.1

#define zoom   0.800
#define tile   0.850
#define speed  0.010

#define brightness 0.0015
#define darkmatter 0.300
#define distfading 0.730
#define saturation 0.850

// in vec2 fragCoord;
out vec4 fragColor;

void main()
{
  vec2 fragCoord = gl_FragCoord.xy;
	//get coords and direction
	vec2 uv=fragCoord.xy/resolution.xy-.5;
	uv.y*=resolution.y/resolution.x;
	vec3 dir=vec3(uv*zoom,1.);
	// float time=time*speed+.25;

	//mouse rotation
	float a1=.5;
	float a2=.8;
	// mat2 rot1=mat2(0.8775825619,0.4794255386,-0.4794255386,0.8775825619);
	mat2 rot1=mat2(1,0,0,1);
	mat2 rot2=mat2(1,0,0,1);
	// dir.xz*=rot1;
	// dir.xy*=rot2;
	// vec3 from=vec3(1.,.5,0.5);
	// from+=vec3(time,time,-2.);
	// from.xz*=rot1;
	// from.xy*=rot2;

	vec3 origin = vec3(origin, 0);

	//volumetric rendering
	float s=0.1,fade=1.;
	vec3 v=vec3(0.);
	for (int r=0; r<volsteps; r++) {
		// vec3 p=from+s*dir*.5;
		vec3 p = origin + dir * s * 0.5;
		p = abs(vec3(tile)-mod(p,vec3(tile*2.))); // tiling fold
		float pa,a=pa=0.;
		for (int i=0; i<iterations; i++) {
			p=abs(p)/dot(p,p)-formuparam; // the magic formula
			a+=abs(length(p)-pa); // absolute sum of average change
			pa=length(p);
		}
		float dm=max(0.,darkmatter-a*a*.001); //dark matter
		a*=a*a; // add contrast
		if (r>6) fade*=1.-dm; // dark matter, don't render near
		//v+=vec3(dm,dm*.5,0.);
		v+=fade;
		v+=vec3(s,s*s,s*s*s*s)*a*brightness*fade; // coloring based on distance
		fade*=distfading; // distance fading
		s+=stepsize;
	}
	v=mix(vec3(length(v)),v,saturation); //color adjust
	fragColor = vec4(v*.01,1.);

}
