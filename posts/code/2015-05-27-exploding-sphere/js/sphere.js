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

    var renderer = new THREE.WebGLRenderer({ alpha: true, antialias: (width > 1024) });
    renderer.setSize(width, height);
    console.log(width, height);
    renderer.setClearColor(0xffffff, 0);
    canvas.appendChild(renderer.domElement);

    var headerHeight = 72;

    function onMouseMove( event ) {
            // calculate mouse position in normalized device coordinates
            // (-1 to +1) for both components
        mouse.x = event.clientX / window.innerWidth * 2 - 1;
        mouse.y = - (event.clientY / (window.innerHeight + headerHeight)) * 2 + 1;
    }

    function onTouchMove(e) {
        console.log('tm');
        if (e.changedTouches.length > 0) {
            mouse.x = (e.changedTouches[0].pageX / window.innerWidth) * 2 - 1;
            mouse.y = - (e.changedTouches[0].pageY / (window.innerHeight + headerHeight)) * 2 + 1;
        }
        e.preventDefault();
    }

    function onTouchEnd() {
        mouse.x = 1;
        mouse.y = 1;
    }


    function onResize(event){
        width = window.innerWidth;
        height = window.innerHeight;
        renderer.setSize(width, height);
        camera = new THREE.PerspectiveCamera(60, width/height, 0.1, 100);
        camera.position.z = 6;
    }

    document.addEventListener('mousemove', onMouseMove, false);
    renderer.domElement.addEventListener('touchmove', onTouchMove, false);
    renderer.domElement.addEventListener('touchend', onTouchEnd, false);
    renderer.setPixelRatio(window.devicePixelRatio);
    window.addEventListener('resize', onResize, true);

    this.material = new ExplosiveMaterial(center);
    var mesh = new THREE.Mesh(extIco, material.material);
    scene.add(mesh);

    var render = function () {
        requestAnimationFrame(render);
        raycaster.setFromCamera(mouse, camera);

        var intersects = raycaster.intersectObjects([mesh]);

        material.setIntersection(intersects);

        renderer.render(scene, camera);
    };

    render();
    return this;
}
