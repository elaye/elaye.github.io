// For the landing page
// To merge with sphere.js

var ExplodingSphere = function(canvas, x, y, z){
	var center = new THREE.Vector3(x, y, z);
	var width = window.innerWidth;
	var height = window.innerHeight;

	var mouse = new THREE.Vector2(-width/2, height/2);
	var scene = new THREE.Scene();
	var camera = new THREE.PerspectiveCamera(60, width/height, 0.1, 100);
	camera.position.z = 6;
	var raycaster = new THREE.Raycaster();

	var extIco = new ExtrudedIcosphere(center);

	var renderer = new THREE.WebGLRenderer({alpha: true, antialias: true});
	renderer.setSize(width, height);
	renderer.setClearColor(0xffffff, 0);
	canvas.appendChild(renderer.domElement);

	function onMouseMove( event ) {
	       // calculate mouse position in normalized device coordinates
	       // (-1 to +1) for both components
	       mouse.x = event.clientX / width * 2 - 1;
	       mouse.y = - (event.clientY / height) * 2 + 1;
	}

	function onResize(event){
		width = window.innerWidth;
		height = window.innerHeight;
		renderer.setSize(width, height);
		camera = new THREE.PerspectiveCamera(60, width/height, 0.1, 100);
		camera.position.z = 6;
	}

	document.addEventListener('mousemove', onMouseMove, false);
	window.addEventListener('resize', onResize, true);

	this.material = new ExplosiveMaterial(center);
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

		renderer.render(scene, camera);
	};

	render();
	return this;
}
