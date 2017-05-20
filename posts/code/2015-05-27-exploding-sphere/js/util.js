var Timer = function(){
	this.totalTime = 500;
	this.timeStart = 0;
	this.isOn = false;

	this.start = function(t){
		this.timeStart = Date.now();
		this.totalTime = t;
		this.isOn = true;
	};

	this.getTimeLeft = function(){
		if(!this.isOn){
			return 0;
		}
		var timeLeft = ((this.timeStart + this.totalTime) - Date.now());
		if(timeLeft < 0){
			timeLeft = 0;
			isOn = false;
		}
		return timeLeft;
	};

	this.getTimeLeftNormalized = function(){
		var timeLeftNormalized = this.getTimeLeft() / this.totalTime;	
		return timeLeftNormalized;
	};
}

var shaderUtil = {
	"common": [
		"#define PI 3.141592653589793238462643383279",

		"float fastSqrt(float x){",
			"float s;",
			"return s;",
		"}"
	].join("\n"),

	"distributions": [
		"float gauss(float s, float x){",
			"float a = 1.0/sqrt(2.0 * PI * s * s);",
			"float f = a * exp(-x * x / (2.0 * s * s));",
			"return f;",
		"}",
	].join("\n"),

	"quaternionOperations": [
		// "void rotate(inout vec3 v, vec4 q){",
		// 	"v += 2.0 * cross( cross( v, q.xyz ) + q.w * v, q.xyz );",
		// "}",

		"vec4 randomQuaternion(float u1, float u2, float u3){",
			"vec4 q;",
			"float s1mu1 = sqrt(1.0 - u1);",
			"float su1 = sqrt(u1);",
			"q.x = s1mu1 * sin(2.0 * PI * u2);",
			"q.y = s1mu1 * cos(2.0 * PI * u2);",
			"q.z = su1 * sin(2.0 * PI * u3);",
			"q.w = su1 * cos(2.0 * PI * u3);",
			"return q;",
		"}",

		"vec4 inverseQuaternion(vec4 q){",
			"vec4 qm1;",
			"float d = q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w;",
			"qm1.x = - q.x / d;",
			"qm1.y = - q.y / d;",
			"qm1.z = - q.z / d;",
			"qm1.w = q.w / d;",
			"return qm1;",
		"}",

		// "vec4 quaternionMult(vec4 q, vec4 r){",
		// 	"vec4 t;",
		// 	"t.x = r.x * q.x - r.y * q.y - r.z * q.z - r.w * q.w;",
		// 	"t.y = r.x * q.y + r.y * q.x - r.z * q.w + r.w * q.z;",
		// 	"t.z = r.x * q.z + r.y * q.w + r.z * q.x - r.w * q.y;",
		// 	"t.w = r.x * q.w - r.y * q.z + r.z * q.y + r.w * q.x;",
		// 	"return t;",
		// "}",

		"vec4 multQuat(vec4 q1, vec4 q2){",
			"return vec4(",
				"q1.w * q2.x + q1.x * q2.w + q1.z * q2.y - q1.y * q2.z,",
				"q1.w * q2.y + q1.y * q2.w + q1.x * q2.z - q1.z * q2.x,",
				"q1.w * q2.z + q1.z * q2.w + q1.y * q2.x - q1.x * q2.y,",
				"q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z",
			");",
		"}",

		"vec3 rotate_vector( vec4 quat, vec3 vec ){",
			"vec4 qv = multQuat( quat, vec4(vec, 0.0) );",
			"return multQuat( qv, vec4(-quat.x, -quat.y, -quat.z, quat.w) ).xyz;",
		"}"
	].join("\n"),

	"snoise4D": [
		"//",
		"// Description : Array and textureless GLSL 2D/3D/4D simplex ",
		"//               noise functions.",
		"//      Author : Ian McEwan, Ashima Arts.",
		"//  Maintainer : ijm",
		"//     Lastmod : 20110822 (ijm)",
		"//     License : Copyright (C) 2011 Ashima Arts. All rights reserved.",
		"//               Distributed under the MIT License. See LICENSE file.",
		"//               https://github.com/ashima/webgl-noise",
		"// ",

		"vec4 mod289(vec4 x){",
			"return x - floor(x * (1.0 / 289.0)) * 289.0;",
		"}",

		"float mod289(float x){",
			"return x - floor(x * (1.0 / 289.0)) * 289.0;",
		"}",

		"vec4 permute(vec4 x){",
			"return mod289(((x*34.0)+1.0)*x);",
		"}",

		"float permute(float x){",
			"return mod289(((x*34.0)+1.0)*x);",
		"}",

		"vec4 taylorInvSqrt(vec4 r){",
			"return 1.79284291400159 - 0.85373472095314 * r;",
		"}",

		"float taylorInvSqrt(float r){",
			"return 1.79284291400159 - 0.85373472095314 * r;",
		"}",

		"vec4 grad4(float j, vec4 ip){",
			"const vec4 ones = vec4(1.0, 1.0, 1.0, -1.0);",
			"vec4 p,s;",

			"p.xyz = floor( fract (vec3(j) * ip.xyz) * 7.0) * ip.z - 1.0;",
			"p.w = 1.5 - dot(abs(p.xyz), ones.xyz);",
			"s = vec4(lessThan(p, vec4(0.0)));",
			"p.xyz = p.xyz + (s.xyz*2.0 - 1.0) * s.www; ",

			"return p;",
		"}",

		"// (sqrt(5) - 1)/4 = F4, used once below",
		"#define F4 0.309016994374947451",

		"float snoise(vec4 v){",
			"const vec4  C = vec4( 0.138196601125011,  // (5 - sqrt(5))/20  G4",
									"0.276393202250021,  // 2 * G4",
									"0.414589803375032,  // 3 * G4",
									"-0.447213595499958); // -1 + 4 * G4",
			"// First corner",
			"vec4 i  = floor(v + dot(v, vec4(F4)) );",
			"vec4 x0 = v -   i + dot(i, C.xxxx);",
			"// Other corners",
			"// Rank sorting originally contributed by Bill Licea-Kane, AMD (formerly ATI)",
			"vec4 i0;",
			"vec3 isX = step( x0.yzw, x0.xxx );",
			"vec3 isYZ = step( x0.zww, x0.yyz );",
			"//  i0.x = dot( isX, vec3( 1.0 ) );",
			"i0.x = isX.x + isX.y + isX.z;",
			"i0.yzw = 1.0 - isX;",
			"//  i0.y += dot( isYZ.xy, vec2( 1.0 ) );",
			"i0.y += isYZ.x + isYZ.y;",
			"i0.zw += 1.0 - isYZ.xy;",
			"i0.z += isYZ.z;",
			"i0.w += 1.0 - isYZ.z;",

			"// i0 now contains the unique values 0,1,2,3 in each channel",
			"vec4 i3 = clamp( i0, 0.0, 1.0 );",
			"vec4 i2 = clamp( i0-1.0, 0.0, 1.0 );",
			"vec4 i1 = clamp( i0-2.0, 0.0, 1.0 );",

			"//  x0 = x0 - 0.0 + 0.0 * C.xxxx",
			"//  x1 = x0 - i1  + 1.0 * C.xxxx",
			"//  x2 = x0 - i2  + 2.0 * C.xxxx",
			"//  x3 = x0 - i3  + 3.0 * C.xxxx",
			"//  x4 = x0 - 1.0 + 4.0 * C.xxxx",
			"vec4 x1 = x0 - i1 + C.xxxx;",
			"vec4 x2 = x0 - i2 + C.yyyy;",
			"vec4 x3 = x0 - i3 + C.zzzz;",
			"vec4 x4 = x0 + C.wwww;",

			"// Permutations",
			"i = mod289(i); ",
			"float j0 = permute( permute( permute( permute(i.w) + i.z) + i.y) + i.x);",
			"vec4 j1 = permute( permute( permute( permute (",
					" i.w + vec4(i1.w, i2.w, i3.w, 1.0 ))",
					"+ i.z + vec4(i1.z, i2.z, i3.z, 1.0 ))",
					"+ i.y + vec4(i1.y, i2.y, i3.y, 1.0 ))",
					"+ i.x + vec4(i1.x, i2.x, i3.x, 1.0 ));",

			"// Gradients: 7x7x6 points over a cube, mapped onto a 4-cross polytope",
			"// 7*7*6 = 294, which is close to the ring size 17*17 = 289.",
			"vec4 ip = vec4(1.0/294.0, 1.0/49.0, 1.0/7.0, 0.0) ;",

			"vec4 p0 = grad4(j0,   ip);",
			"vec4 p1 = grad4(j1.x, ip);",
			"vec4 p2 = grad4(j1.y, ip);",
			"vec4 p3 = grad4(j1.z, ip);",
			"vec4 p4 = grad4(j1.w, ip);",

			"// Normalise gradients",
			"vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));",
			"p0 *= norm.x;",
			"p1 *= norm.y;",
			"p2 *= norm.z;",
			"p3 *= norm.w;",
			"p4 *= taylorInvSqrt(dot(p4,p4));",

			"// Mix contributions from the five corners",
			"vec3 m0 = max(0.6 - vec3(dot(x0,x0), dot(x1,x1), dot(x2,x2)), 0.0);",
			"vec2 m1 = max(0.6 - vec2(dot(x3,x3), dot(x4,x4)), 0.0);",
			"m0 = m0 * m0;",
			"m1 = m1 * m1;",
			"return 49.0 * ( dot(m0*m0, vec3( dot( p0, x0 ), dot( p1, x1 ), dot( p2, x2 )))",
				"+ dot(m1*m1, vec2( dot( p3, x3 ), dot( p4, x4 ) ) ) ) ;",
		"}"
	].join("\n")

}