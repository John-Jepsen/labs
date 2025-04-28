# Filtering and Convolution Lab

## Objective

Understand and implement image filtering and convolution operations using OpenCV. This lab explores how kernels (small matrices) applied to images can create various effects like blurring, sharpening, and edge enhancement.

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

### Convolution

Convolution is a mathematical operation that applies a kernel (small matrix) to an image to produce a new image. The process involves:

1. Placing the kernel over each pixel in the input image
2. Multiplying overlapping elements
3. Summing the products
4. Replacing the center pixel with the sum

![Convolution Operation](https://example.com/placeholder)

### Kernels

A kernel (also called a filter or mask) is a small matrix used during convolution. Different kernels produce different effects:

| Kernel Type     | Effect              | Example Application                  |
| :-------------- | :------------------ | :----------------------------------- |
| Box Filter      | Blur/Smooth         | Noise reduction                      |
| Gaussian Filter | Smooth with weights | Reduce detail while preserving edges |
| Sharpen         | Enhance edges       | Highlight details                    |
| Sobel           | Detect gradients    | Edge detection                       |
| Laplacian       | Detect edges        | Second-derivative edge detection     |

### Example Kernels

```
# Box Blur (3x3)
[1/9, 1/9, 1/9]
[1/9, 1/9, 1/9]
[1/9, 1/9, 1/9]

# Sharpen
[ 0, -1,  0]
[-1,  5, -1]
[ 0, -1,  0]

# Sobel (horizontal)
[-1, 0, 1]
[-2, 0, 2]
[-1, 0, 1]
```

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
        tuple: (bgr_image, rgb_image)
    """
    # Load the image
    img = cv2.imread(image_path)
    if img is None:
        raise FileNotFoundError(f"Could not load image from {image_path}")

    # Convert to RGB for matplotlib display
    img_rgb = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)

    return img, img_rgb

def display_images(images, titles, figsize=(15, 10)):
    """
    Display multiple images in a grid.

    Args:
        images (list): List of images to display
        titles (list): List of titles for each image
        figsize (tuple): Figure size
    """
    n = len(images)
    fig, axes = plt.subplots(1, n, figsize=figsize)

    if n == 1:
        axes = [axes]

    for i, (img, title) in enumerate(zip(images, titles)):
        axes[i].imshow(img)
        axes[i].set_title(title)
        axes[i].axis('off')

    plt.tight_layout()
    plt.show()

def apply_box_blur(image, kernel_size=3):
    """
    Apply a box blur filter to an image.

    Args:
        image (np.ndarray): Input image
        kernel_size (int): Size of the kernel (3, 5, etc.)

    Returns:
        np.ndarray: Blurred image
    """
    # TODO: Implement box blur using cv2.blur()

    # Convert back to RGB if the input was RGB
    if len(image.shape) == 3:
        blurred_rgb = cv2.cvtColor(blurred, cv2.COLOR_BGR2RGB)
        return blurred_rgb
    return blurred

def apply_gaussian_blur(image, kernel_size=3, sigma=0):
    """
    Apply a Gaussian blur filter to an image.

    Args:
        image (np.ndarray): Input image
        kernel_size (int): Size of the kernel (must be odd)
        sigma (float): Standard deviation of the Gaussian

    Returns:
        np.ndarray: Blurred image
    """
    # TODO: Implement Gaussian blur using cv2.GaussianBlur()

    # Convert back to RGB if the input was RGB
    if len(image.shape) == 3:
        blurred_rgb = cv2.cvtColor(blurred, cv2.COLOR_BGR2RGB)
        return blurred_rgb
    return blurred

def apply_median_blur(image, kernel_size=3):
    """
    Apply a median blur filter to an image.

    Args:
        image (np.ndarray): Input image
        kernel_size (int): Size of the kernel (must be odd)

    Returns:
        np.ndarray: Blurred image
    """
    # TODO: Implement median blur using cv2.medianBlur()

    # Convert back to RGB if the input was RGB
    if len(image.shape) == 3:
        blurred_rgb = cv2.cvtColor(blurred, cv2.COLOR_BGR2RGB)
        return blurred_rgb
    return blurred

def apply_bilateral_filter(image, diameter=9, sigma_color=75, sigma_space=75):
    """
    Apply bilateral filter to an image (edge-preserving smoothing).

    Args:
        image (np.ndarray): Input image
        diameter (int): Diameter of each pixel neighborhood
        sigma_color (float): Filter sigma in the color space
        sigma_space (float): Filter sigma in the coordinate space

    Returns:
        np.ndarray: Filtered image
    """
    # TODO: Implement bilateral filter using cv2.bilateralFilter()

    # Convert back to RGB if the input was RGB
    if len(image.shape) == 3:
        filtered_rgb = cv2.cvtColor(filtered, cv2.COLOR_BGR2RGB)
        return filtered_rgb
    return filtered

def apply_custom_kernel(image, kernel):
    """
    Apply a custom kernel to an image using the filter2D function.

    Args:
        image (np.ndarray): Input image
        kernel (np.ndarray): Custom kernel (must be odd-sized)

    Returns:
        np.ndarray: Filtered image
    """
    # TODO: Implement custom kernel application using cv2.filter2D()

    # Convert back to RGB if the input was RGB
    if len(image.shape) == 3:
        filtered_rgb = cv2.cvtColor(filtered, cv2.COLOR_BGR2RGB)
        return filtered_rgb
    return filtered

def create_sharpen_kernel():
    """
    Create a sharpening kernel.

    Returns:
        np.ndarray: Sharpen kernel
    """
    # TODO: Define and return a sharpening kernel

    return kernel

def create_edge_kernel(kernel_type='sobel_x'):
    """
    Create an edge detection kernel.

    Args:
        kernel_type (str): Type of kernel ('sobel_x', 'sobel_y', 'laplacian')

    Returns:
        np.ndarray: Edge detection kernel
    """
    # TODO: Define and return the specified edge detection kernel

    return kernel

def visualize_kernel(kernel, figsize=(5, 5)):
    """
    Visualize a kernel as a heatmap.

    Args:
        kernel (np.ndarray): Kernel to visualize
        figsize (tuple): Figure size
    """
    plt.figure(figsize=figsize)
    plt.imshow(kernel, cmap='viridis')
    plt.colorbar(label='Weight')
    plt.title(f'Kernel Shape: {kernel.shape}')
    for i in range(kernel.shape[0]):
        for j in range(kernel.shape[1]):
            plt.text(j, i, f'{kernel[i, j]:.2f}',
                     ha='center', va='center',
                     color='white' if abs(kernel[i, j]) < 0.5 else 'black')
    plt.tight_layout()
    plt.show()

def compare_blur_methods(image):
    """
    Compare different blur methods side by side.

    Args:
        image (np.ndarray): Input image in BGR format
    """
    # TODO: Apply different blur methods and display results side by side

    pass

def examine_kernel_effects(image):
    """
    Examine the effects of different kernels on an image.

    Args:
        image (np.ndarray): Input image in BGR format
    """
    # TODO: Apply different custom kernels and display results

    pass

# Main execution
if __name__ == "__main__":
    # Sample image path - replace with your own
    image_path = "sample_image.jpg"

    # Load image
    img_bgr, img_rgb = load_image(image_path)

    # Display original image
    plt.figure(figsize=(10, 8))
    plt.imshow(img_rgb)
    plt.title('Original Image')
    plt.axis('off')
    plt.show()

    # TODO: Demonstrate kernel visualizations and filtering effects
```

---

## Group Implementation Tasks

### Task 1: Complete the Missing Functions

Working in pairs, complete each TODO section in the starter code:

- Person A: Implement blur filters (box, Gaussian, median)
- Person B: Implement bilateral filter and custom kernel application

### Task 2: Kernel Exploration

1. Create at least three different custom kernels (beyond those mentioned)
2. Apply each kernel to the same image
3. Document the visual effect and underlying mathematical reason for each effect

Example table to complete:

| Kernel               | Matrix Values                     | Visual Effect  | Mathematical Explanation                           |
| :------------------- | :-------------------------------- | :------------- | :------------------------------------------------- |
| Sharpen              | [0,-1,0]<br>[-1,5,-1]<br>[0,-1,0] | Enhances edges | Amplifies center pixel while subtracting neighbors |
| Your Custom Kernel 1 |                                   |                |                                                    |
| Your Custom Kernel 2 |                                   |                |                                                    |
| Your Custom Kernel 3 |                                   |                |                                                    |

### Task 3: Noise Reduction Analysis

1. Add different types of noise to an image (salt-and-pepper, Gaussian)
2. Apply different blur filters to each noisy image
3. Compare the effectiveness of each filter for each noise type
4. Measure quality using visual inspection and a numeric metric

---

## Peer Review Questions

After completing the implementation, review each other's code and discuss:

1. **Implementation Choices**:

   - Which parameters work best for each filter type?
   - How does kernel size affect the output quality and processing time?

2. **Filter Comparisons**:

   - Which blur filter preserves edges better?
   - For which scenarios would you choose median vs. Gaussian blur?

3. **Kernel Understanding**:

   - What happens if kernel weights sum to zero? To one? To values greater than one?
   - How do kernel dimensions affect the output?

4. **Performance Considerations**:
   - Which filters are computationally more expensive? Why?
   - How might filter operations be optimized for large images?

---

## Troubleshooting Guide

### Common Issues

1. **Kernel Size Errors**

   - Kernel sizes must be odd numbers (3, 5, 7, etc.)
   - Larger kernels cause stronger effects but are slower

2. **Edge Artifacts**

   - Border pixels may show artifacts due to incomplete neighborhood
   - Use border handling techniques like cv2.BORDER_REFLECT

3. **Type Conversion Issues**

   - Ensure image data types are consistent for operations
   - Check if operation expects uint8 (0-255) or float (0.0-1.0)

4. **Performance Problems**
   - Large kernels can be computationally expensive
   - Consider using optimized versions or separable kernels

### Debugging Visualization

Create a function that shows:

1. The original pixel neighborhood (e.g., 5×5 region)
2. The applied kernel
3. The multiplication step
4. The final sum/result

---

## Mini-Project: Custom Image Filter Application

Create an application that:

1. Loads an image from a file or camera
2. Allows the user to design custom kernels
3. Previews the effect of the kernel in real-time
4. Provides a library of common filters with explanations
5. Supports filter chaining (applying multiple filters in sequence)

### Extension Ideas

- Add noise generation and removal tools
- Implement frequency domain filtering (FFT-based)
- Create a "smart sharpen" that adapts to image content
- Develop a tool that suggests optimal kernel parameters

---

## Reflection Questions

1. How do kernel operations relate to human visual perception?
2. What's the relationship between kernel size and the amount of information lost?
3. How might convolution be used in other computer vision tasks beyond filtering?
4. What are the limitations of kernel-based operations?
5. How do modern deep learning approaches compare to traditional kernel methods?
