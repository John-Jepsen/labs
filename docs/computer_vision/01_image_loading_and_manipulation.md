# Image Loading and Manipulation Lab

## Objective

Master the fundamentals of loading, inspecting, and manipulating digital images using OpenCV and NumPy. This lab introduces the core building blocks needed for all computer vision projects.

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

### Images as Matrices

Digital images are represented as multi-dimensional arrays:

- Grayscale: 2D array (height × width)
- Color: 3D array (height × width × channels)

![Image Matrix Representation](https://example.com/placeholder)

### Color Spaces

Images can be represented in different color systems:

- **RGB**: Red, Green, Blue channels (most common)
- **BGR**: Blue, Green, Red (OpenCV's default format)
- **HSV**: Hue, Saturation, Value (useful for color filtering)
- **Grayscale**: Single channel intensity

---

## Starter Code

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt

def load_and_display(image_path):
    """
    Load an image and display it using matplotlib.

    Args:
        image_path (str): Path to the image file

    Returns:
        np.ndarray: The loaded image
    """
    # TODO: Load the image using cv2.imread
    # Remember that OpenCV loads images in BGR format

    # TODO: Convert BGR to RGB for correct display in matplotlib

    # Display the image
    plt.figure(figsize=(10, 8))
    plt.imshow(img_rgb)
    plt.axis('off')
    plt.title('Original Image')
    plt.show()

    return img

def inspect_image(image):
    """
    Print basic information about an image.

    Args:
        image (np.ndarray): Image to inspect
    """
    # TODO: Print shape, data type, min/max values

    # TODO: If color image, print information for each channel

def resize_image(image, width=None, height=None, scale=None):
    """
    Resize an image based on width, height, or scale factor.

    Args:
        image (np.ndarray): Input image
        width (int, optional): Target width
        height (int, optional): Target height
        scale (float, optional): Scale factor

    Returns:
        np.ndarray: Resized image
    """
    # TODO: Implement resizing logic
    # If scale is provided, resize proportionally
    # If width and height are provided, resize to those dimensions
    # If only width or only height is provided, maintain aspect ratio

    return resized_image

def crop_image(image, x, y, width, height):
    """
    Crop a region from the image.

    Args:
        image (np.ndarray): Input image
        x, y (int): Top-left corner coordinates
        width, height (int): Width and height of crop region

    Returns:
        np.ndarray: Cropped image
    """
    # TODO: Implement cropping logic

    return cropped_image

def convert_colorspace(image, target_space='rgb'):
    """
    Convert image between color spaces.

    Args:
        image (np.ndarray): Input image
        target_space (str): Target color space ('rgb', 'gray', 'hsv')

    Returns:
        np.ndarray: Converted image
    """
    # TODO: Implement color space conversion
    # Support at least RGB, grayscale, and HSV

    return converted_image

def flip_image(image, direction='horizontal'):
    """
    Flip an image horizontally or vertically.

    Args:
        image (np.ndarray): Input image
        direction (str): 'horizontal', 'vertical', or 'both'

    Returns:
        np.ndarray: Flipped image
    """
    # TODO: Implement image flipping

    return flipped_image

def rotate_image(image, angle):
    """
    Rotate an image by the specified angle.

    Args:
        image (np.ndarray): Input image
        angle (float): Rotation angle in degrees

    Returns:
        np.ndarray: Rotated image
    """
    # TODO: Implement image rotation around the center

    return rotated_image

# Main execution block
if __name__ == "__main__":
    # Sample image path - replace with your own
    image_path = "sample_image.jpg"

    # Load and display the image
    img = load_and_display(image_path)

    # Inspect the image
    inspect_image(img)

    # Demonstrate image manipulations
    # TODO: Add code to demonstrate each function and visualize results
```

---

## Group Implementation Tasks

### Task 1: Complete the Missing Functions

Working in pairs, complete each TODO section in the starter code. Divide the tasks between team members:

- Person A: Implement loading, inspection, and resizing functions
- Person B: Implement cropping, color conversion, and transformation functions

### Task 2: Create a Function Showcase

Develop a demonstration function that:

1. Loads an image of your choice
2. Displays it alongside a montage of different manipulations
3. Shows the effect of at least 4 different operations

Example showcase layout:

```
[Original Image] | [Resized Image]
[Grayscale Image] | [Cropped Image]
[Flipped Image] | [Rotated Image]
```

### Task 3: Image Matrix Investigation

1. Select a small region (e.g., 5×5 pixels) from an image
2. Print the raw pixel values before and after manipulation
3. Discuss how the numerical values change with each operation

---

## Peer Review Questions

After completing the implementation, review each other's code and discuss:

1. **Implementation Approaches**:

   - How did your approaches to implementing the functions differ?
   - Which implementation is more efficient or readable?

2. **Visual Results Analysis**:

   - How does the quality of resized images compare between different methods?
   - What happens to image details when converting between color spaces?

3. **Matrix Understanding**:

   - What happens to the pixel values when you flip or rotate an image?
   - How does cropping affect the image matrix dimensions?

4. **Edge Cases**:
   - How does your code handle images of different formats (PNG vs JPG)?
   - What happens if the crop region extends beyond the image boundaries?

---

## Troubleshooting Guide

### Common Issues

1. **Image Not Loading**

   - Check if the file path is correct and the file exists
   - Verify that the file format is supported by OpenCV

2. **Color Distortion**

   - Remember that OpenCV uses BGR order, not RGB
   - Ensure proper conversion when displaying with matplotlib

3. **Dimension Errors**

   - Check array shapes before operations
   - Verify that crop coordinates are within image boundaries

4. **Memory Issues**
   - Be cautious with very large images
   - Release resources with `cv2.destroyAllWindows()`

---

## Mini-Project: Image Transformation Tool

Create a simple command-line tool that:

1. Takes an input image path and output directory
2. Applies a user-selected sequence of transformations
3. Saves the transformed images with descriptive filenames
4. Generates a visual report showing all transformations

### Extension Ideas

- Add a simple GUI with sliders for transformation parameters
- Implement batch processing for multiple images
- Create custom transformation presets (e.g., "Thumbnail generator", "Social media formatter")

---

## Reflection Questions

1. How might these basic operations be used in real-world applications?
2. What happens to image quality after multiple transformations?
3. How would you optimize these operations for very large images?
4. What additional metadata might be useful to extract from images?
5. How do digital cameras and smartphones implement similar operations?
