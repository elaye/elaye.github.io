var shaders = {
	"vertex": [
			"precision highp float;",
			shaderUtil["common"],
			shaderUtil["distributions"],
			shaderUtil["snoise4D"],
			shaderUtil["quaternionOperations"],
			"uniform vec3 center;",
			"uniform vec3 intPos;",
			"uniform vec3 lightPos1;",
			"uniform vec3 lightPos2;",
			"uniform float time;",
			"uniform float bMouseOver;",
			// "uniform float mouseOverCnt;",
			"uniform float mouseOutCnt;",
			"uniform float reconstructCnt;",

			"uniform float normalAmp;",
			"uniform float lateralAmp;",
			"uniform float normalDev;",
			"uniform float lateralDev;",

			"uniform float noisePositionScale;",
			"uniform float noiseTimeScale;",
			"uniform float noiseAmp;",

			"uniform float rewindSpeed;",

			"uniform vec3 diffuseColor1;",
			"uniform vec3 diffuseColor2;",

			"attribute vec3 faceCenter;",
			"attribute vec3 faceNormal;",

			"varying vec4 varyingDiffuse;",

			"float easeOut(float x, float a){",
				"float t = 1.0 - exp(-x * a);",
				"return t;",
			"}",

			"void displace(float d, inout vec3 p){",
				"float rewind = easeOut(reconstructCnt, rewindSpeed);",
				// Normal displacement
				"float normalDisAmp = bMouseOver * rewind * normalAmp * gauss(normalDev, d);",
				"vec3 normalDis = normalDisAmp * faceNormal;",

				// Lateral displacement
				"float lateralDisAmp = bMouseOver * rewind * lateralAmp * exp( (normalDisAmp - 1.0) * lateralDev);",
				"vec3 lateralDir = cross(-intPos, cross(intPos, faceCenter));",
				"vec3 lateralDis = lateralDisAmp * lateralDir;",

				// Noise
				"vec3 noiseDir = normalize(p - center);",
				"vec3 noise = noiseAmp * snoise(vec4(noisePositionScale * p.xy, noiseTimeScale * mouseOutCnt, noiseTimeScale * rewind)) * noiseDir;",

				"p += normalDis + lateralDis + noise;",
			"}",

			"vec4 randomQuaternion(vec4 v){",
				"float u = snoise(v);",
				"u = (u + 1.0) / 2.0;",
				"return randomQuaternion(u, u, u);",
			"}",

			"void randomRotate(float d, inout vec3 p){",
				"p -= faceCenter;",
				"float amp = bMouseOver * 50.0 * exp(-d * d * 4.0);",

				"vec4 q0 = randomQuaternion(vec4(0.0, faceCenter));",
				"vec4 q = randomQuaternion(vec4(amp, faceCenter));",

				"p = rotate_vector(inverseQuaternion(q0), p);",
				"p = rotate_vector(q, p);",

				"p += faceCenter;",
			"}",

			"vec4 diffuseLight(vec3 color, vec3 lightPos){",
				"float cos = dot(normal,normalize(lightPos));",
				"cos = clamp(cos, 0.0, 1.0);",
				"return cos * vec4(color, 1.0);",
			"}",

			"void main(){",
				"float d = distance(normal, intPos - center);",

				"vec3 newPosition = position;",
				"randomRotate(d, newPosition);",
				"displace(d, newPosition);",
				
				"gl_Position = projectionMatrix * modelViewMatrix * vec4(newPosition, 1.0);",

				"varyingDiffuse = diffuseLight(diffuseColor1, lightPos1);",
				"varyingDiffuse += diffuseLight(diffuseColor2, lightPos2);",
			"}"
	].join("\n"),

	"fragment": [
			"uniform vec2 resolution;",
			"uniform float time;",

			"uniform vec3 ambientColor;",

			"varying vec4 varyingDiffuse;",

			"void main(){",
				"vec4 ambient = vec4(ambientColor, 1.0);",
				"vec4 diffuse = varyingDiffuse;",

				"vec4 color = ambient + diffuse;",
				"gl_FragColor = color;",
			"}"
	].join("\n")
}