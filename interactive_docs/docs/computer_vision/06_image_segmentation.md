# Image Segmentation Lab

## Objective

Learn to segment images by dividing them into meaningful regions or objects. This lab covers basic segmentation techniques that form the foundation for more advanced computer vision applications like object detection, scene understanding, and image editing.

---

## Environment Setup

| Dependency   | Purpose                            |
| :----------- | :--------------------------------- |
| Python 3.10+ | Programming language               |
| OpenCV       | Image processing library           |
| NumPy        | Numerical operations for arrays    |
| Matplotlib   | Visualization of images            |
| scikit-image | Additional segmentation algorithms |

### Installation

```bash
# Create virtual environment (optional)
python -m venv cv_env
source cv_env/bin/activate  # On Windows: cv_env\Scripts\activate

# Install required packages
pip install opencv-python numpy matplotlib scikit-image
```

### Verification

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt
from skimage import segmentation

# Verify versions
print(f"OpenCV version: {cv2.__version__}")
print(f"NumPy version: {np.__version__}")
print(f"scikit-image version: {segmentation.__version__}")
```

---

## Key Concepts

### What is Image Segmentation?

Image segmentation is the process of partitioning an image into multiple segments or regions, each with similar attributes. Unlike edge detection or contour finding, segmentation aims to create a complete division of the image.

![Segmentation Example](https://example.com/placeholder)

### Main Approaches

| Method         | Description                                      | Best Used For                             |
| :------------- | :----------------------------------------------- | :---------------------------------------- |
| Thresholding   | Segment based on pixel intensity                 | Simple backgrounds, high contrast         |
| Color-based    | Group similar colors (e.g., K-means)             | Objects with distinct colors              |
| Region-growing | Expand from seed points                          | Cohesive regions with gradual transitions |
| Watershed      | Treat gradient magnitudes as topographic surface | Touching objects, cell segmentation       |
| GrabCut        | Interactive segmentation with minimal user input | Foreground extraction, detailed objects   |

### Applications

- Medical image analysis (tumor detection, organ segmentation)
- Object extraction and background removal
- Autonomous driving (road/obstacle detection)
- Video surveillance (foreground/background separation)
- Image editing and composition

---

## Starter Code

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt
from skimage import segmentation, color
import random

def load_image(image_path):
    """
    Load an image and convert to RGB for display.

    Args:
        image_path (str): Path to the image file

    Returns:
        tuple: (bgr_image, rgb_image)
    """
    # Load the image
    img = cv2.imread(image_path)
    if img is None:
        raise FileNotFoundError(f"Could not load image from {image_path}")

    # Convert to RGB for matplotlib display
    img_rgb = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)

    return img, img_rgb

def display_images(images, titles, figsize=(15, 10), rows=1):
    """
    Display multiple images in a grid.

    Args:
        images (list): List of images to display
        titles (list): List of titles for each image
        figsize (tuple): Figure size
        rows (int): Number of rows in the grid
    """
    n = len(images)
    cols = (n + rows - 1) // rows  # Ceiling division
    fig, axes = plt.subplots(rows, cols, figsize=figsize)

    # Make axes a 2D array if it isn't already
    if rows == 1 and cols == 1:
        axes = np.array([[axes]])
    elif rows == 1:
        axes = axes.reshape(1, -1)
    elif cols == 1:
        axes = axes.reshape(-1, 1)

    # Flatten both arrays for easy iteration
    axes_flat = axes.flatten()

    for i, (img, title) in enumerate(zip(images, titles)):
        if i < len(axes_flat):
            if len(img.shape) == 2:  # Grayscale
                axes_flat[i].imshow(img, cmap='gray')
            else:  # Color image
                axes_flat[i].imshow(img)
            axes_flat[i].set_title(title)
            axes_flat[i].axis('off')

    # Hide unused subplots
    for i in range(n, len(axes_flat)):
        axes_flat[i].axis('off')

    plt.tight_layout()
    plt.show()

def apply_binary_thresholding(image, threshold=127, max_value=255):
    """
    Apply binary thresholding to segment an image.

    Args:
        image (np.ndarray): Grayscale input image
        threshold (int): Threshold value
        max_value (int): Maximum value to use

    Returns:
        np.ndarray: Binary segmented image
    """
    # TODO: Implement binary thresholding using cv2.threshold

    return thresholded_image

def apply_otsu_thresholding(image, max_value=255):
    """
    Apply Otsu's thresholding to automatically determine the optimal threshold.

    Args:
        image (np.ndarray): Grayscale input image
        max_value (int): Maximum value to use

    Returns:
        tuple: (segmented image, optimal threshold)
    """
    # TODO: Implement Otsu's thresholding using cv2.threshold with THRESH_OTSU flag

    return thresholded_image, optimal_threshold

def apply_adaptive_thresholding(image, block_size=11, c=2, max_value=255):
    """
    Apply adaptive thresholding to handle images with varying illumination.

    Args:
        image (np.ndarray): Grayscale input image
        block_size (int): Size of pixel neighborhood for threshold calculation
        c (int): Constant subtracted from mean or weighted sum
        max_value (int): Maximum value to use

    Returns:
        np.ndarray: Segmented image
    """
    # TODO: Implement adaptive thresholding using cv2.adaptiveThreshold

    return thresholded_image

def apply_color_based_segmentation(image, n_clusters=5, attempts=10):
    """
    Apply color-based segmentation using K-means clustering.

    Args:
        image (np.ndarray): BGR input image
        n_clusters (int): Number of clusters/segments
        attempts (int): Number of attempts for K-means

    Returns:
        np.ndarray: Segmented image with each pixel replaced by the cluster center
    """
    # TODO: Implement color-based segmentation using K-means
    # 1. Reshape the image to a 2D array of pixels
    # 2. Convert to float32
    # 3. Apply K-means
    # 4. Replace each pixel with its cluster center
    # 5. Reshape back to original image dimensions

    return segmented_image

def apply_watershed_segmentation(image):
    """
    Apply watershed segmentation to separate touching objects.

    Args:
        image (np.ndarray): BGR input image

    Returns:
        np.ndarray: Segmented image with watershed regions
    """
    # TODO: Implement watershed segmentation
    # 1. Convert to grayscale
    # 2. Apply binary thresholding or other preprocessing
    # 3. Compute distance transform
    # 4. Find markers (local maxima of distance transform)
    # 5. Apply watershed

    return segmented_image

def apply_grabcut_segmentation(image, rect=None):
    """
    Apply GrabCut algorithm for foreground extraction.

    Args:
        image (np.ndarray): BGR input image
        rect (tuple): Rectangle containing foreground (x, y, width, height)

    Returns:
        np.ndarray: Foreground mask and segmented image
    """
    # TODO: Implement GrabCut segmentation
    # 1. Define rectangle for foreground
    # 2. Create initial mask
    # 3. Create temporary arrays for algorithm
    # 4. Run GrabCut algorithm
    # 5. Create output mask and apply to image

    if rect is None:
        # If no rectangle provided, use a default centered one
        height, width = image.shape[:2]
        margin = min(width, height) // 4
        rect = (margin, margin, width - 2*margin, height - 2*margin)

    return mask, segmented_image

def apply_region_growing_segmentation(image, seeds=None, threshold=10):
    """
    Apply region growing segmentation from seed points.

    Args:
        image (np.ndarray): Grayscale input image
        seeds (list): List of seed points [(x1, y1), (x2, y2), ...]
        threshold (int): Intensity threshold for region growing

    Returns:
        np.ndarray: Segmented image with regions
    """
    # TODO: Implement simple region growing algorithm
    # 1. Initialize mask for output
    # 2. For each seed, grow region by considering neighbors
    # 3. Add neighbors to region if within threshold of region mean

    if seeds is None:
        # If no seeds provided, use a default grid
        height, width = image.shape[:2]
        seeds = [(width//4, height//4), (3*width//4, height//4),
                 (width//4, 3*height//4), (3*width//4, 3*height//4)]

    return segmented_image

def apply_felzenszwalb_segmentation(image, scale=100, sigma=0.5, min_size=50):
    """
    Apply Felzenszwalb's segmentation algorithm.

    Args:
        image (np.ndarray): RGB input image
        scale (float): Free parameter. Higher means larger clusters.
        sigma (float): Gaussian kernel width for pre-smoothing
        min_size (int): Minimum component size

    Returns:
        np.ndarray: Segmented image with colored regions
    """
    # TODO: Implement Felzenszwalb's algorithm using skimage

    return segmented_image

def apply_slic_superpixels(image, n_segments=100, compactness=10):
    """
    Apply SLIC superpixel segmentation.

    Args:
        image (np.ndarray): RGB input image
        n_segments (int): Approximate number of segments
        compactness (float): Balances color and space proximity

    Returns:
        np.ndarray: Segmented image with superpixel boundaries
    """
    # TODO: Implement SLIC superpixel segmentation using skimage

    return segmented_image

def create_segmentation_comparison(image):
    """
    Compare different segmentation methods side by side.

    Args:
        image (np.ndarray): BGR input image

    Returns:
        list: List of (segmented_image, method_name) tuples
    """
    # TODO: Apply different segmentation methods and collect results

    return []

def evaluate_segmentation(segmented_image, ground_truth=None):
    """
    Evaluate segmentation quality using metrics.

    Args:
        segmented_image (np.ndarray): Segmented image
        ground_truth (np.ndarray, optional): Ground truth segmentation

    Returns:
        dict: Dictionary of evaluation metrics
    """
    # TODO: Implement segmentation evaluation
    # If ground truth available, compute overlap metrics
    # Otherwise, calculate internal evaluation metrics (e.g., segment homogeneity)

    metrics = {
        'num_segments': 0,
        'avg_segment_size': 0,
        'segment_size_std': 0
    }

    return metrics

def visualize_segments(segmented_image, original_image=None, random_colors=True):
    """
    Visualize segmentation results with colored regions.

    Args:
        segmented_image (np.ndarray): Segmented image with integer labels
        original_image (np.ndarray, optional): Original image for overlay
        random_colors (bool): Whether to use random colors or a colormap

    Returns:
        np.ndarray: Visualization image
    """
    # TODO: Implement segmentation visualization
    # 1. Create a color map for segments
    # 2. Map each segment to a color
    # 3. Optionally overlay on original image

    return visualization

# Main execution
if __name__ == "__main__":
    # Sample image path - replace with your own
    image_path = "sample_image.jpg"

    # Load image
    img_bgr, img_rgb = load_image(image_path)

    # Convert to grayscale for methods that require it
    img_gray = cv2.cvtColor(img_bgr, cv2.COLOR_BGR2GRAY)

    # Display original image
    plt.figure(figsize=(10, 8))
    plt.imshow(img_rgb)
    plt.title('Original Image')
    plt.axis('off')
    plt.show()

    # TODO: Demonstrate segmentation methods
```

---

## Group Implementation Tasks

### Task 1: Complete the Missing Functions

Working in pairs, complete each TODO section in the starter code:

- Person A: Implement thresholding methods and color-based segmentation
- Person B: Implement watershed, region growing, and advanced segmentation methods

### Task 2: Segmentation Comparison

1. Apply at least 5 different segmentation methods to a set of diverse images:

   - A simple object with clear boundaries
   - A complex scene with multiple objects
   - A natural scene (e.g., landscape)
   - A texture-rich image (e.g., fabric, foliage)
   - A medical image (if available)

2. Create a visual comparison matrix and document which methods work best for each image type:

| Image Type    | Best Method | Worst Method | Notes |
| :------------ | :---------- | :----------- | :---- |
| Simple object |             |              |       |
| Complex scene |             |              |       |
| Natural scene |             |              |       |
| Texture-rich  |             |              |       |
| Medical       |             |              |       |

### Task 3: Parameter Experimentation

1. Choose one segmentation method (e.g., K-means or watershed)
2. Vary key parameters and document their effect on segmentation quality
3. Create a visual grid showing parameter impact

Example experiment:

| Parameter            | Value 1 | Value 2 | Value 3 |
| :------------------- | :------ | :------ | :------ |
| K-means clusters (k) | k=3     | k=5     | k=10    |
| [Visual Result]      | [Image] | [Image] | [Image] |

---

## Peer Review Questions

After completing the implementation, review each other's code and discuss:

1. **Method Selection**:

   - Which segmentation method would you choose for medical image analysis?
   - When would you prefer thresholding over clustering-based methods?

2. **Parameter Sensitivity**:

   - How sensitive is each method to parameter changes?
   - Which parameters had the largest impact on results?

3. **Performance Analysis**:

   - Which methods are computationally more efficient?
   - How does image size affect processing time for different methods?

4. **Result Quality**:

   - How would you measure segmentation quality without ground truth?
   - What visual artifacts or common problems did you observe?

5. **Practical Applications**:
   - How might you combine multiple segmentation techniques for better results?
   - What preprocessing steps improved segmentation quality?

---

## Troubleshooting Guide

### Common Issues

1. **Over-segmentation**

   - Too many small regions created
   - Increase clustering parameters (k for K-means)
   - Apply smoothing before segmentation
   - Use region merging as post-processing

2. **Under-segmentation**

   - Important objects merged together
   - Decrease threshold values
   - Increase number of clusters/segments
   - Apply edge-preserving smoothing

3. **Noisy Results**

   - Segments are fragmented and inconsistent
   - Apply noise reduction preprocessing
   - Use morphological operations to clean results
   - Consider marker-based watershed

4. **Boundary Inaccuracy**
   - Segment boundaries don't align with object edges
   - Combine with edge detection
   - Use gradient information in segmentation
   - Try GrabCut or graph-based methods

### Debugging Tools

Create functions that:

1. Highlight segmentation boundaries on the original image
2. Show region properties (size, average color, texture)
3. Compare multiple segmentation results side by side

---

## Mini-Project: Interactive Segmentation Tool

Create an application that:

1. Loads images from files
2. Provides a user interface to select and configure segmentation methods
3. Allows parameter adjustment with real-time preview
4. Lets users refine segmentation results manually
5. Extracts segment properties and statistics
6. Exports segmentation results as masks or separate images

### Extension Ideas

- Implement segment tracking across multiple frames of video
- Add automatic object labeling based on segment properties
- Create a tool that combines multiple segmentation results
- Develop a segmentation-based image editing tool (e.g., selective color adjustment)
- Build a medical image analysis tool for specific applications

---

## Reflection Questions

1. How do higher-level semantic understanding and lower-level segmentation relate?
2. What challenges remain in image segmentation that deep learning approaches address?
3. How might contextual information improve segmentation results?
4. What are the trade-offs between interactive and fully automatic segmentation?
5. How has image segmentation evolved to handle increasingly complex scenes?
