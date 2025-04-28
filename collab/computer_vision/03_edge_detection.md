# Edge Detection Lab

## Objective

Understand and implement various edge detection techniques using OpenCV. This lab explores how edges can be detected, compared, and utilized in computer vision applications.

---

## Environment Setup

| Dependency   | Purpose                         |
| :----------- | :------------------------------ |
| Python 3.10+ | Programming language            |
| OpenCV       | Image processing library        |
| NumPy        | Numerical operations for arrays |
| Matplotlib   | Visualization of images         |

### Installation

```bash
# Create virtual environment (optional)
python -m venv cv_env
source cv_env/bin/activate  # On Windows: cv_env\Scripts\activate

# Install required packages
pip install opencv-python numpy matplotlib
```

### Verification

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt

# Verify versions
print(f"OpenCV version: {cv2.__version__}")
print(f"NumPy version: {np.__version__}")
```

---

## Key Concepts

### What are Edges?

Edges in images are areas with strong intensity contrasts – a sharp change in intensity from one pixel to the next. They often represent:

- Boundaries between objects
- Changes in surface orientation or material properties
- Depth discontinuities
- Changes in scene illumination

![Edge Examples](https://example.com/placeholder)

### Edge Detection Process

Edge detection typically involves three stages:

1. **Noise Reduction**: Smooth the image to reduce noise (often with Gaussian blur)
2. **Gradient Calculation**: Compute intensity gradients to find regions of rapid intensity change
3. **Edge Tracing/Thresholding**: Apply thresholds to determine which gradients represent actual edges

### Common Techniques

| Method    | Description                                    | Strengths                 | Weaknesses                            |
| :-------- | :--------------------------------------------- | :------------------------ | :------------------------------------ |
| Sobel     | Calculates gradients using convolution         | Simple, detects direction | Sensitive to noise                    |
| Canny     | Multi-stage algorithm with hysteresis          | More precise, less noise  | More complex, parameter tuning needed |
| Laplacian | Second derivative operator                     | Detects edges and corners | Very sensitive to noise               |
| Scharr    | Modified Sobel with better rotational symmetry | Better angular accuracy   | Similar limitations to Sobel          |

---

## Starter Code

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt

def load_image(image_path):
    """
    Load an image and convert to RGB for display.

    Args:
        image_path (str): Path to the image file

    Returns:
        tuple: (bgr_image, rgb_image, grayscale_image)
    """
    # Load the image
    img = cv2.imread(image_path)
    if img is None:
        raise FileNotFoundError(f"Could not load image from {image_path}")

    # Convert to RGB for matplotlib display
    img_rgb = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)

    # Convert to grayscale
    img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

    return img, img_rgb, img_gray

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
    if rows == 1:
        axes = axes.reshape(1, -1)
    elif n == 1:
        axes = np.array([[axes]])

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

def apply_gaussian_blur(image, kernel_size=5, sigma=0):
    """
    Apply Gaussian blur for noise reduction.

    Args:
        image (np.ndarray): Input image
        kernel_size (int): Size of the kernel (must be odd)
        sigma (float): Standard deviation of the Gaussian

    Returns:
        np.ndarray: Blurred image
    """
    # TODO: Implement Gaussian blur pre-processing

    return blurred_image

def sobel_edge_detection(image, ksize=3):
    """
    Apply Sobel edge detection.

    Args:
        image (np.ndarray): Grayscale input image
        ksize (int): Kernel size

    Returns:
        tuple: (gradient_x, gradient_y, gradient_magnitude, gradient_direction)
    """
    # TODO: Implement Sobel edge detection
    # - Calculate gradients in x and y directions
    # - Calculate gradient magnitude and direction

    return grad_x, grad_y, magnitude, direction

def scharr_edge_detection(image):
    """
    Apply Scharr edge detection.

    Args:
        image (np.ndarray): Grayscale input image

    Returns:
        tuple: (gradient_x, gradient_y, gradient_magnitude, gradient_direction)
    """
    # TODO: Implement Scharr edge detection
    # - Calculate gradients in x and y directions
    # - Calculate gradient magnitude and direction

    return grad_x, grad_y, magnitude, direction

def laplacian_edge_detection(image, ksize=3):
    """
    Apply Laplacian edge detection.

    Args:
        image (np.ndarray): Grayscale input image
        ksize (int): Kernel size

    Returns:
        np.ndarray: Laplacian edges
    """
    # TODO: Implement Laplacian edge detection

    return laplacian

def canny_edge_detection(image, low_threshold=50, high_threshold=150, aperture_size=3):
    """
    Apply Canny edge detection.

    Args:
        image (np.ndarray): Grayscale input image
        low_threshold (int): Lower threshold for hysteresis
        high_threshold (int): Upper threshold for hysteresis
        aperture_size (int): Aperture size for Sobel operator

    Returns:
        np.ndarray: Canny edges
    """
    # TODO: Implement Canny edge detection

    return edges

def compare_edge_detectors(image):
    """
    Compare different edge detection methods side by side.

    Args:
        image (np.ndarray): Grayscale input image
    """
    # TODO: Apply different edge detection methods and display results

    pass

def threshold_edges(edges, threshold=127):
    """
    Apply binary thresholding to edge images.

    Args:
        edges (np.ndarray): Edge detection output
        threshold (int): Threshold value

    Returns:
        np.ndarray: Binary edge image
    """
    # TODO: Implement thresholding for edge images

    return binary_edges

def visualize_gradient_directions(magnitude, direction):
    """
    Visualize gradient directions using color coding.

    Args:
        magnitude (np.ndarray): Gradient magnitude
        direction (np.ndarray): Gradient direction in radians

    Returns:
        np.ndarray: Colorized direction visualization
    """
    # TODO: Implement gradient direction visualization
    # Hint: Convert angle to hue, magnitude to value in HSV color space

    return direction_visualization

def adaptive_edge_detection(image, method='canny', params=None):
    """
    Apply edge detection with adaptive parameter selection.

    Args:
        image (np.ndarray): Grayscale input image
        method (str): Edge detection method ('canny', 'sobel', etc.)
        params (dict): Optional parameters to override defaults

    Returns:
        np.ndarray: Edge detection result
    """
    # TODO: Implement adaptive parameter selection based on image statistics

    return edges

# Main execution
if __name__ == "__main__":
    # Sample image path - replace with your own
    image_path = "sample_image.jpg"

    # Load image
    img_bgr, img_rgb, img_gray = load_image(image_path)

    # Display original image
    plt.figure(figsize=(10, 8))
    plt.imshow(img_rgb)
    plt.title('Original Image')
    plt.axis('off')
    plt.show()

    # TODO: Demonstrate edge detection methods and comparison
```

---

## Group Implementation Tasks

### Task 1: Complete the Missing Functions

Working in pairs, complete each TODO section in the starter code:

- Person A: Implement Sobel and Laplacian edge detection functions
- Person B: Implement Canny edge detection and gradient visualization functions

### Task 2: Parameter Experimentation

1. Choose one image and apply the Canny edge detector with different parameters
2. Create a matrix of results showing how changing thresholds affects edge detection
3. Document the optimal parameters for:
   - A high-contrast architectural image
   - A low-contrast natural scene
   - A busy, textured image (like fabric or foliage)

Example experiment table:

| Parameter Set    | Low Threshold | High Threshold | Aperture Size | Result Image | Observations             |
| :--------------- | :------------ | :------------- | :------------ | :----------- | :----------------------- |
| Default          | 50            | 150            | 3             | [Image]      | Baseline detection       |
| Low sensitivity  | 100           | 200            | 3             | [Image]      | Missing subtle edges     |
| High sensitivity | 30            | 100            | 3             | [Image]      | More noise, more details |
| ...              |               |                |               |              |                          |

### Task 3: Edge Detection Applications

Implement one of these practical applications:

1. **Shape Counter**: Count shapes in an image using edge detection and contour finding
2. **Document Scanner**: Detect document boundaries using edge detection
3. **License Plate Finder**: Locate rectangular license plates in vehicle images
4. **Panorama Stitcher**: Find features for image stitching using edge information

---

## Peer Review Questions

After completing the implementation, review each other's code and discuss:

1. **Method Comparison**:

   - Which edge detection method produces the cleanest results?
   - When would you choose Sobel over Canny, or vice versa?

2. **Parameter Sensitivity**:

   - How sensitive is each method to parameter changes?
   - Which parameters had the largest impact on the results?

3. **Processing Steps**:

   - What effect does pre-processing (blur) have on edge detection?
   - How does post-processing (thresholding) affect the final result?

4. **Application Considerations**:

   - How well would these edge detectors work in low-light conditions?
   - What pre-processing would help with noisy images?

5. **Performance Analysis**:
   - Which methods are computationally more efficient?
   - How does image size affect processing time?

---

## Troubleshooting Guide

### Common Issues

1. **Too Many Edges Detected**

   - Image is too noisy or textured
   - Threshold values are too low
   - Apply Gaussian blur before edge detection
   - Increase threshold values

2. **Missing Important Edges**

   - Threshold values are too high
   - Pre-processing may be removing important details
   - Decrease threshold values
   - Try adaptive thresholding

3. **Broken or Disconnected Edges**

   - Typical with simple methods like Sobel
   - Try Canny edge detection which includes hysteresis thresholding
   - Post-process with morphological operations

4. **Performance Issues**
   - Edge detection can be slow on large images
   - Downscale the image before processing
   - Use optimized implementations (hardware acceleration)

### Debugging Tools

Create a function that shows:

1. Original image alongside the edge detection result
2. Gradient magnitude histogram to help select appropriate thresholds
3. Zoomed view of specific regions to examine edge quality

---

## Mini-Project: Edge-Based Feature Detection

Create an application that:

1. Detects and counts objects in an image using edge detection
2. Classifies detected objects by shape (circle, square, triangle, etc.)
3. Measures object dimensions and distances between objects
4. Visualizes results with annotations and statistics

### Extension Ideas

- Implement real-time edge detection using webcam input
- Create an edge-based image stylization filter (cartoon, sketch)
- Build a simple optical character recognition (OCR) system for printed text
- Develop an edge-based image compression technique

---

## Reflection Questions

1. How do different lighting conditions affect edge detection results?
2. Why might combining multiple edge detection techniques yield better results?
3. How does the human visual system detect edges compared to algorithms?
4. What role does edge detection play in more complex computer vision tasks?
5. How might deep learning approaches to edge detection differ from traditional methods?
