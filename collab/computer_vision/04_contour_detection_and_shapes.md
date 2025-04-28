# Contour Detection and Shape Analysis Lab

## Objective

Learn to detect, analyze, and classify shapes in images using contour detection techniques. This lab builds on edge detection knowledge to identify and measure distinct objects in images.

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

### What are Contours?

Contours are continuous curves that follow the boundaries of objects in an image. They represent shapes at constant intensity levels and are fundamental for:

- Object detection and counting
- Shape analysis and recognition
- Feature extraction and measurement
- Image segmentation

![Contour Examples](https://example.com/placeholder)

### Contour Hierarchy

Contours can have parent-child relationships:

- **External contours**: Outline the outer boundaries of objects
- **Internal contours**: Represent holes or nested objects within other contours

### Shape Descriptors

Various metrics can be used to analyze and classify shapes:

- **Area and Perimeter**: Basic size measurements
- **Aspect Ratio**: Width to height ratio
- **Extent**: Ratio of contour area to bounding rectangle area
- **Solidity**: Ratio of contour area to convex hull area
- **Moments**: Shape statistics used for centroid calculation and more
- **Hu Moments**: Shape descriptors invariant to rotation, scale, and reflection

---

## Starter Code

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt
import math

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

def preprocess_for_contours(image, blur_ksize=5, threshold_method='binary'):
    """
    Preprocess an image for contour detection.

    Args:
        image (np.ndarray): Grayscale input image
        blur_ksize (int): Kernel size for Gaussian blur
        threshold_method (str): Thresholding method ('binary', 'otsu', 'adaptive')

    Returns:
        np.ndarray: Binary image ready for contour detection
    """
    # TODO: Implement image preprocessing for contour detection
    # 1. Apply Gaussian blur to reduce noise
    # 2. Apply appropriate thresholding based on method parameter

    return binary_image

def find_contours(binary_image, retrieval_mode=cv2.RETR_EXTERNAL):
    """
    Find contours in a binary image.

    Args:
        binary_image (np.ndarray): Binary input image
        retrieval_mode (int): Contour retrieval mode

    Returns:
        tuple: (list of contours, hierarchy)
    """
    # TODO: Implement contour finding
    # Use cv2.findContours() with appropriate parameters

    return contours, hierarchy

def draw_contours(image, contours, color=(0, 255, 0), thickness=2):
    """
    Draw contours on an image.

    Args:
        image (np.ndarray): Image to draw on
        contours (list): Contours to draw
        color (tuple): BGR color
        thickness (int): Line thickness

    Returns:
        np.ndarray: Image with contours drawn
    """
    # TODO: Implement contour drawing
    # Create a copy of the image and draw contours on it

    return img_with_contours

def calculate_shape_descriptors(contour):
    """
    Calculate various shape descriptors for a contour.

    Args:
        contour (np.ndarray): Input contour

    Returns:
        dict: Dictionary of shape descriptors
    """
    # TODO: Implement shape descriptor calculations
    # Include area, perimeter, aspect ratio, extent, solidity, etc.

    return descriptors

def identify_shape(contour, epsilon_factor=0.04):
    """
    Identify the basic shape of a contour.

    Args:
        contour (np.ndarray): Input contour
        epsilon_factor (float): Factor for polygon approximation

    Returns:
        str: Identified shape ('circle', 'triangle', 'square', 'rectangle', etc.)
    """
    # TODO: Implement shape identification
    # Use approxPolyDP to approximate the contour to a polygon
    # Based on the number of vertices, identify the shape

    return shape_name

def filter_contours(contours, min_area=100, max_area=None):
    """
    Filter contours by area.

    Args:
        contours (list): List of contours
        min_area (float): Minimum area threshold
        max_area (float, optional): Maximum area threshold

    Returns:
        list: Filtered contours
    """
    # TODO: Implement contour filtering by area

    return filtered_contours

def extract_shape_features(image, contours):
    """
    Extract features from each shape in the image.

    Args:
        image (np.ndarray): Original image
        contours (list): List of contours

    Returns:
        list: List of shape features (descriptors, color, etc.)
    """
    # TODO: Implement feature extraction for each contour
    # Calculate shape descriptors and extract color information

    return shape_features

def classify_shapes(contours):
    """
    Classify contours into shape categories.

    Args:
        contours (list): List of contours

    Returns:
        dict: Dictionary mapping shape types to contour indices
    """
    # TODO: Implement shape classification
    # Group contours by their identified shapes

    return shape_categories

def visualize_shape_analysis(image, contours, features):
    """
    Visualize shape analysis with annotations.

    Args:
        image (np.ndarray): Original image
        contours (list): List of contours
        features (list): List of shape features

    Returns:
        np.ndarray: Annotated image
    """
    # TODO: Implement visualization with annotations
    # Draw contours, labels, and key measurements

    return annotated_image

# Main execution
if __name__ == "__main__":
    # Sample image path - replace with your own
    image_path = "sample_shapes.jpg"

    # Load image
    img_bgr, img_rgb, img_gray = load_image(image_path)

    # Display original image
    plt.figure(figsize=(10, 8))
    plt.imshow(img_rgb)
    plt.title('Original Image')
    plt.axis('off')
    plt.show()

    # TODO: Demonstrate contour detection and shape analysis
```

---

## Group Implementation Tasks

### Task 1: Complete the Missing Functions

Working in pairs, complete each TODO section in the starter code:

- Person A: Implement preprocessing, contour finding, and contour drawing functions
- Person B: Implement shape descriptor calculation and shape identification functions

### Task 2: Shape Dataset Creation

1. Create a dataset of basic shapes:
   - Take photos of simple shapes (circles, triangles, squares, etc.)
   - Draw shapes digitally using drawing tools
   - Generate synthetic shapes with Python
2. Test your contour detection and shape classification on this dataset
3. Document the accuracy of your classification

### Task 3: Real-World Shape Detection

Choose one of these practical applications and implement it:

1. **Board Game Piece Counter**: Detect and count different game pieces by color and shape
2. **Coin Counter**: Identify and count different coins in an image
3. **Logo Detector**: Find company logos in images based on shape characteristics
4. **Traffic Sign Detector**: Detect and classify traffic signs by shape

---

## Peer Review Questions

After completing the implementation, review each other's code and discuss:

1. **Preprocessing Effectiveness**:

   - How does the choice of thresholding method affect contour detection?
   - What preprocessing steps worked best for different image types?

2. **Shape Descriptor Performance**:

   - Which shape descriptors were most reliable for classification?
   - How invariant are your descriptors to rotation and scale changes?

3. **Contour Hierarchy**:

   - How would you handle nested shapes (shapes within shapes)?
   - What's the best way to represent parent-child relationships?

4. **Edge Cases and Challenges**:

   - How well does your code handle touching or overlapping shapes?
   - What strategies could improve shape detection in complex images?

5. **Optimization Considerations**:
   - Which steps in the process are most computationally expensive?
   - How might the algorithm be optimized for real-time applications?

---

## Troubleshooting Guide

### Common Issues

1. **Poor Contour Detection**

   - Image has low contrast or is noisy
   - Thresholding parameters are not appropriate
   - Try adaptive thresholding or adjust blur parameters
   - Use morphological operations (opening/closing) to clean up binary image

2. **Inaccurate Shape Classification**

   - Contour approximation epsilon is too large or small
   - Shape criteria are too strict or lenient
   - Adjust epsilon factor for polygon approximation
   - Implement tolerance ranges for shape metrics

3. **Missing Small Objects**

   - Minimum area threshold is too high
   - Small objects are being merged with noise reduction
   - Lower area threshold or adjust preprocessing parameters
   - Use more targeted noise reduction techniques

4. **Merged Objects**
   - Objects are touching or overlapping
   - Try watershed segmentation for touching objects
   - Use distance transform and markers for separation
   - Consider convexity defects for splitting merged shapes

### Debugging Tools

Create functions that visualize:

1. Each preprocessing step side-by-side
2. Contour hierarchies with color-coding
3. Shape approximation polygons overlaid on original contours
4. Feature values as bar charts for each detected shape

---

## Mini-Project: Object Counter and Classifier

Create an application that:

1. Takes an image containing multiple objects
2. Segments and identifies each distinct object
3. Classifies objects by shape and color
4. Counts objects in each category
5. Generates a summary report with visualizations

Example output:

```
Image Analysis Report:
- Total objects detected: 15
- Circles: 5 (3 red, 2 blue)
- Squares: 4 (2 green, 2 yellow)
- Triangles: 6 (4 red, 2 green)
```

### Extension Ideas

- Add area and perimeter measurements for each object
- Implement object tracking across video frames
- Create a spatial relationship analyzer (which objects are near others)
- Build a simple picking robot simulator that plans grasping based on shape

---

## Reflection Questions

1. How does lighting affect contour detection and shape analysis?
2. What challenges arise when detecting irregular or complex shapes?
3. How might machine learning improve shape classification compared to geometric rules?
4. What real-world applications could benefit from contour-based shape analysis?
5. How would you approach detecting partially occluded shapes?
