
# Lispworks 5 on Linux doesn't want to rescale the images in the
# visualizer, so do it here.
small-images:
	for image in idle processing receiving sending waiting; do \
		convert $${image}.bmp -resize 24x24 $${image}-small.bmp; \
	done
