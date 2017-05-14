// For the demo page
// To merge with sphere1.js

var raycaster = new THREE.Raycaster();
var mouse = new THREE.Vector2(-window.innerWidth/2, window.innerHeight/2);
var scene = new THREE.Scene();
var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 100 );
camera.position.z = 5;

var center = new THREE.Vector3(0, 0, 0);
var extIco = new ExtrudedIcosphere(center);

var renderer = new THREE.WebGLRenderer({antialias: true});
renderer.setSize( window.innerWidth, window.innerHeight );
renderer.setClearColor( 0xfafafa, 1);
// renderer.gammaOutput = true;
document.body.appendChild(renderer.domElement);

window.onload = function() {
  // var opt = new Options();
	var gui = new dat.GUI({ autoPlace: false });
	var guiContainer = document.getElementById('gui');
	guiContainer.appendChild(gui.domElement);

	var displacement = gui.addFolder('Displacement');
	displacement.add(material, 'normalAmp', 0, 1000);
	displacement.add(material, 'normalDev', 0.2, 0.4);
	displacement.add(material, 'lateralAmp', 0, 1);
	displacement.add(material, 'lateralDev', 0, 20);

	var noise = gui.addFolder('Noise');
	noise.add(material, 'noisePositionScale', 0.1, 3.0);
	noise.add(material, 'noiseTimeScale', 0.1, 10.0);
	noise.add(material, 'noiseAmp', 0.01, 1.0);

	var explosion = gui.addFolder('Explosion');
	explosion.add(material, 'rewindSpeed', 0.5, 5.0);

	var lights = gui.addFolder('Lights');
	lights.addColor(material, 'ambientColor');
	lights.addColor(material, 'diffuseColor1');
	lights.addColor(material, 'diffuseColor2');
};

function onMouseMove( event ) {
       // calculate mouse position in normalized device coordinates
       // (-1 to +1) for both components
       mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
       mouse.y = - (event.clientY / window.innerHeight) * 2 + 1;
}

document.addEventListener('mousemove', onMouseMove, false);

var material = new ExplosiveMaterial(center);
var mesh = new THREE.Mesh(extIco, material.material);
scene.add(mesh);

var render = function () {
	requestAnimationFrame(render);
	// update the picking ray with the camera and mouse position
	raycaster.setFromCamera(mouse, camera);

	// calculate objects intersecting the picking ray
	// var intersects = raycaster.intersectObjects(scene.children);
	var intersects = raycaster.intersectObjects([mesh]);

	material.setIntersection(intersects);

	// !!! When to reset time?

	// if(!mouseOver){
		// mesh.rotation.y += 0.005;
	// }
	// cube.rotation.x += 0.1;
	// meshes[0].rotation.y += 0.01;
	// renderer.antialias = true;
	// mesh.rotation.y += 0.01;
	renderer.render(scene, camera);

};

render();
