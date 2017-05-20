var ExplosiveMaterial = function(center){

	this.mouseOver = false;
	this.intPos = new THREE.Vector3();

	this.reconstructTimer = new Timer();
	this.reconstructDuration = 500;

	this.normalAmp = 300.0;
	this.normalDev = 0.3;
	this.lateralAmp = 1;
	this.lateralDev = 10;

	this.noisePositionScale = 1.4;
	this.noiseTimeScale = 1.0;
	this.noiseAmp = 0.2;
	// this.noiseAmp = 0;

	this.rewindSpeed = 3.0;

	this.ambientColor = [79, 79, 92];
	this.diffuseColor1 = [97, 132, 156];
	this.diffuseColor2 = [181, 143, 112];

	var uniforms = {
		time: { type: "f", value: 0 },
		resolution: { type: "v2", value: new THREE.Vector2(512.0, 512.0) },
		center: {type: "v3", value: center},
		intPos: {type: "v3", value: this.intPos },
		lightPos1: {type: "v3", value: new THREE.Vector3(2.6, 1.9, 1.7)},
		lightPos2: {type: "v3", value: new THREE.Vector3(-3.2, 2.6, 1.1)},
		bMouseOver: {type: "f", value: 0},
		mouseOutCnt: {type: "f", value: 0},
		reconstructCnt: {type: "f", value: 0},

		normalAmp: {type: "f", value: this.normalAmp},
		lateralAmp: {type: "f", value: this.lateralAmp},
		normalDev: {type: "f", value: this.normalDev},
		lateralDev: {type: "f", value: this.lateralDev},

		noisePositionScale: {type: "f", value: this.noisePositionScale},
		noiseTimeScale: {type: "f", value: this.noiseTimeScale},
		noiseAmp: {type: "f", value: this.noiseAmp},

		rewindSpeed: {type: "f", value: this.rewindSpeed},

		ambientColor: {type: "v3", value: new THREE.Vector3(
						this.ambientColor[0]/255,
						this.ambientColor[1]/255,
						this.ambientColor[2]/255
					)},
		diffuseColor1: {type: "v3", value: new THREE.Vector3(
						this.diffuseColor1[0]/255,
						this.diffuseColor1[1]/255,
						this.diffuseColor1[2]/255
					)},
		diffuseColor2: {type: "v3", value: new THREE.Vector3(
						this.diffuseColor2[0]/255,
						this.diffuseColor2[1]/255,
						this.diffuseColor2[2]/255
					)}
	};

	this.material = new THREE.ShaderMaterial( {
		uniforms: uniforms,
		attributes: {
			faceNormal: {type: "v3", value: []},
			faceCenter: {type: "v3", value: []}
		},
		vertexShader: shaders["vertex"],
		fragmentShader: shaders["fragment"]
	});

	this.material.side = THREE.DoubleSide;

	this.setIntersection = function(intersects){
		if(intersects.length > 0){
			this.intPos = intersects[0].point;
			this.mouseOver = true;
		}
		else{
			if(this.mouseOver === true){
				this.reconstructTimer.start(this.reconstructDuration);
			}
			this.mouseOver = false;
		}		
		this.updateUniforms();
	};

	this.updateUniforms = function(){
		uniforms.time.value += 0.01;
		uniforms.intPos.value = this.intPos;

		if(this.mouseOver === true){
			uniforms.bMouseOver.value = 1;
			uniforms.reconstructCnt.value = 1;
		}
		else{
			var timeLeft = this.reconstructTimer.getTimeLeftNormalized();
			if(timeLeft > 0){
				uniforms.bMouseOver.value = 1;
			}
			else{
				uniforms.bMouseOver.value = 0;
			}
			uniforms.reconstructCnt.value = timeLeft;
			uniforms.mouseOutCnt.value += 0.01;
		}

		uniforms.normalAmp.value = this.normalAmp;
		uniforms.lateralAmp.value = this.lateralAmp;
		uniforms.normalDev.value = this.normalDev;
		uniforms.lateralDev.value = this.lateralDev;

		uniforms.noisePositionScale.value = this.noisePositionScale;
		uniforms.noiseTimeScale.value = this.noiseTimeScale;
		uniforms.noiseAmp.value = this.noiseAmp;

		uniforms.rewindSpeed.value = this.rewindSpeed;

		uniforms.ambientColor.value = new THREE.Vector3(
						this.ambientColor[0]/255,
						this.ambientColor[1]/255,
						this.ambientColor[2]/255
					);
		uniforms.diffuseColor1.value = new THREE.Vector3(
						this.diffuseColor1[0]/255,
						this.diffuseColor1[1]/255,
						this.diffuseColor1[2]/255
					);
		uniforms.diffuseColor2.value = new THREE.Vector3(
						this.diffuseColor2[0]/255,
						this.diffuseColor2[1]/255,
						this.diffuseColor2[2]/255
					);	
	}

}