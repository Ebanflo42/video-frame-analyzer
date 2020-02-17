# video-frame-analyzer

This was an experiment to see whether or not topological data analysis is a suitable tool for detecting differences in consecutive frames of a video.

It uses the `ffmpeg` tool to convert `webm`s to a collection of `png`s. It then uses the Linux command `convert` to downsize each frame, and the Haskell library `stb-image-redux` to load the images. The images are analyzed as point clouds in 5 dimensional space, where each pixel is a point corresponding to its xy coordinates and rgb values. Using the authors `Persistence` library, these point clouds are converted into filtrations in parallel, and the persistent homology of each filtration is computed in parallel. Finally, the bottleneck distance between the persistence diagrams of consecutive frames is measured - this is meant to be a measure of how qualitatively "different" one frame is from the next.

On an 8-core Intel i7-6700HQ CPU running at 2.6 GHz, the frames had to be downsized to 48x32 pixels in order to be processed in any reasonable amount of time. In addition, the scales for computing persistent homology had to be kept below 0.2 (all coordinates were normalized to lie between 0 and 1).

The difference between consecutive scales was kept minimal as a means to maximize bottleneck distance. Despite this, the distances were alway measured to be zero. *Perhaps* this is because too much detail was lost once the frames were downsized.

If this is the case, perhaps someone with higher-end hardware would like to pick up this project.

## by the way

There is a paper about optimizing persistent homology calculation for cubical complexes, i.e. pixel/voxel based data: https://eurocg11.inf.ethz.ch/abstracts/22.pdf. If you are interested in the idea of this repo and would like to see it come to fruition, you will almost certainly have to use this optimization in place of the ordinary persistence algorithm. Unfortunately, I do not have time to try this myself because this project doesn't pertain to my research or coursework at all.
