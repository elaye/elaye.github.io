## Workflow

A post must be self-contained. Structure:

yyyy-mm-dd-post-name
|- index.html
|- images/
|- resources/
|  |- images/

### Images

Add raw image to `posts/<post-name>/resources/images/`. 
Hakyll will generate images at different resolutions.

Images inside the `resources/images` directory will be converted to different sizes for mobile, tablet and desktop. They can be used with the `picture` field.

Things in `resources/` directories are assets that need to be processed before being included in a webpage. It is mainly for images that need to be converted for different devices. It is useful for keeping original images.
Everything in resources must be checked out with git lfs.

### Post metadata

`maths` `(true | false)` whether this post contains LaTeX formulae that need to be rendered. If `true`, the `mathjax` javascript library will be included in the html head.
`notes` `(string)`

### Custom fields

`$rating("3")$` a rating between 0 and 5
`$picture("src", "alt")$` a picture adpating to the device
`$hero("class")$` same as picture, except that the `src` and `alt` attributes come from a post metadata
`$img("src", "alt")$` simple `img` tag
`$grid$` custom nested grid of posts

