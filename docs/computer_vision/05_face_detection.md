# Face Detection Lab

## Objective

Learn to detect and analyze human faces in images using both traditional computer vision techniques and deep learning approaches. This lab explores the foundations of face detection, its applications, and limitations.

---

## Environment Setup

| Dependency      | Purpose                                                 |
| :-------------- | :------------------------------------------------------ |
| Python 3.10+    | Programming language                                    |
| OpenCV          | Image processing and computer vision library            |
| NumPy           | Numerical operations for arrays                         |
| Matplotlib      | Visualization of images                                 |
| dlib (optional) | Additional face detection and facial landmark detection |

### Installation

```bash
# Create virtual environment (optional)
python -m venv cv_env
source cv_env/bin/activate  # On Windows: cv_env\Scripts\activate

# Install required packages
pip install opencv-python numpy matplotlib

# Optional: Install dlib (may require additional system dependencies)
# pip install dlib
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

### What is Face Detection?

Face detection is the computer vision technology that locates and identifies human faces in digital images. It's a specific case of object detection that focuses on finding instances of human faces, regardless of position, scale, orientation, pose, or lighting.

![Face Detection Example](https://example.com/placeholder)

### Main Approaches

| Method        | Description                                                  | Strengths                                | Weaknesses                                  |
| :------------ | :----------------------------------------------------------- | :--------------------------------------- | :------------------------------------------ |
| Haar Cascades | Uses Haar-like features and cascade classifiers              | Fast, low resource usage                 | Less accurate with varied poses, occlusions |
| HOG + SVM     | Histogram of Oriented Gradients with Support Vector Machines | Better with varied poses                 | More computationally intensive than Haar    |
| Deep Learning | CNN-based detectors like MTCNN, SSD, YOLO                    | Most accurate, handles varied conditions | Higher computational requirements           |

### Applications

- Security and surveillance systems
- Photography (auto-focus, exposure)
- Biometric authentication
- Emotion analysis
- Demographic studies
- Social media filters and effects

---

## Starter Code

```python
import cv2
import numpy as np
import matplotlib.pyplot as plt
import os
from pathlib import Path

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
            axes_flat[i].imshow(img)
            axes_flat[i].set_title(title)
            axes_flat[i].axis('off')

    # Hide unused subplots
    for i in range(n, len(axes_flat)):
        axes_flat[i].axis('off')

    plt.tight_layout()
    plt.show()

def load_haar_cascade(cascade_type='face'):
    """
    Load a Haar cascade classifier.

    Args:
        cascade_type (str): Type of cascade to load ('face', 'eyes', 'smile', etc.)

    Returns:
        cv2.CascadeClassifier: Loaded cascade classifier
    """
    # TODO: Implement loading different Haar cascade files
    # Use cv2.data.haarcascades directory to locate the XML files

    return cascade_classifier

def detect_faces_haar(image, scale_factor=1.1, min_neighbors=5, min_size=(30, 30)):
    """
    Detect faces using Haar cascade classifier.

    Args:
        image (np.ndarray): Input image (grayscale or BGR)
        scale_factor (float): Scale factor for the detection algorithm
        min_neighbors (int): Minimum neighbors for detection
        min_size (tuple): Minimum size of detected faces

    Returns:
        tuple: (image with detections drawn, list of face rectangles)
    """
    # TODO: Implement face detection using Haar cascades
    # 1. Convert to grayscale if needed
    # 2. Apply face detection
    # 3. Draw rectangles around detected faces

    return image_with_faces, faces

def detect_faces_dnn(image, confidence_threshold=0.5):
    """
    Detect faces using a pre-trained deep neural network.

    Args:
        image (np.ndarray): Input image in BGR format
        confidence_threshold (float): Minimum confidence for detections

    Returns:
        tuple: (image with detections drawn, list of face rectangles with confidences)
    """
    # TODO: Implement face detection using OpenCV's DNN module
    # 1. Load the pre-trained model
    # 2. Prepare the image (create blob)
    # 3. Run inference
    # 4. Process detections

    return image_with_faces, faces

def compare_detection_methods(image):
    """
    Compare different face detection methods side by side.

    Args:
        image (np.ndarray): Input image in BGR format
    """
    # TODO: Apply different face detection methods and display results

    pass

def detect_facial_features(image, face_rect):
    """
    Detect facial features (eyes, nose, mouth) within a detected face.

    Args:
        image (np.ndarray): Input image
        face_rect (tuple): Face rectangle (x, y, w, h)

    Returns:
        dict: Dictionary of detected facial features
    """
    # TODO: Implement facial feature detection
    # Use appropriate Haar cascades for eyes, nose, mouth

    return facial_features

def analyze_faces(image, faces):
    """
    Analyze detected faces for additional information.

    Args:
        image (np.ndarray): Input image
        faces (list): List of face rectangles

    Returns:
        list: List of face analyses (position, size, etc.)
    """
    # TODO: Implement basic face analysis
    # Calculate position, size, relative position to image

    return face_analyses

def draw_detections(image, detections, detection_type='face'):
    """
    Draw detection results on an image.

    Args:
        image (np.ndarray): Input image
        detections (list): List of detections (rectangles, landmarks, etc.)
        detection_type (str): Type of detection ('face', 'features', etc.)

    Returns:
        np.ndarray: Image with detections drawn
    """
    # TODO: Implement visualization for different detection types

    return annotated_image

def get_model_files(model_type='dnn_face'):
    """
    Get the model files for the specified model type.

    Args:
        model_type (str): Type of model ('dnn_face', 'dlib_landmarks', etc.)

    Returns:
        dict: Dictionary with model file paths
    """
    # Define model directories and filenames
    models_dir = Path("models")

    # Ensure models directory exists
    os.makedirs(models_dir, exist_ok=True)

    # Define model paths for different model types
    model_paths = {
        'dnn_face': {
            'proto': models_dir / "deploy.prototxt",
            'model': models_dir / "res10_300x300_ssd_iter_140000.caffemodel",
            'urls': {
                'proto': "https://raw.githubusercontent.com/opencv/opencv/master/samples/dnn/face_detector/deploy.prototxt",
                'model': "https://raw.githubusercontent.com/opencv/opencv_3rdparty/dnn_samples_face_detector_20170830/res10_300x300_ssd_iter_140000.caffemodel"
            }
        }
    }

    # Check if model files exist, if not, provide instructions to download
    model_info = model_paths.get(model_type)
    if model_info:
        for key, path in model_info.items():
            if key != 'urls' and not path.exists():
                print(f"Missing {model_type} {key} file at {path}")
                print(f"Download from: {model_info['urls'][key]}")
                print(f"Or use: curl -o {path} {model_info['urls'][key]}")

    return model_info

# Main execution
if __name__ == "__main__":
    # Sample image path - replace with your own
    image_path = "sample_faces.jpg"

    # Load image
    img_bgr, img_rgb = load_image(image_path)

    # Display original image
    plt.figure(figsize=(10, 8))
    plt.imshow(img_rgb)
    plt.title('Original Image')
    plt.axis('off')
    plt.show()

    # Check model files
    get_model_files()

    # TODO: Demonstrate face detection methods
```

---

## Group Implementation Tasks

### Task 1: Complete the Missing Functions

Working in pairs, complete each TODO section in the starter code:

- Person A: Implement Haar cascade face detection and facial feature detection
- Person B: Implement DNN-based face detection and comparative analysis

### Task 2: Face Detection Experiments

1. Test face detection on a diverse set of images with:

   - Multiple faces at different distances
   - Various poses (profile, tilted)
   - Different lighting conditions
   - Occlusions (glasses, masks, partial faces)
   - Diverse ethnicities and ages

2. Create a performance matrix documenting success rates for each method:

| Image Type      | Haar Cascade | DNN Model | Notes |
| :-------------- | :----------- | :-------- | :---- |
| Frontal faces   |              |           |       |
| Profile faces   |              |           |       |
| Group photo     |              |           |       |
| Low light       |              |           |       |
| With occlusions |              |           |       |

### Task 3: False Positive Analysis

1. Run face detection on a set of images containing:
   - No faces
   - Face-like patterns (e.g., electrical outlets, patterns in nature)
   - Cartoon or drawn faces
2. Document false positives and tune parameters to reduce them
3. Analyze what visual patterns trigger false detections

---

## Peer Review Questions

After completing the implementation, review each other's code and discuss:

1. **Detection Method Comparison**:

   - Which method performed best overall?
   - What were the tradeoffs between speed and accuracy?

2. **Parameter Sensitivity**:

   - How did changing parameters affect detection results?
   - Which parameters had the largest impact on false positives vs. false negatives?

3. **Edge Cases**:

   - What types of faces were most challenging to detect?
   - How well did the methods handle occlusions or unusual poses?

4. **Computational Requirements**:

   - How did processing time compare between methods?
   - What are the memory requirements for each approach?

5. **Real-World Applications**:
   - For a real-time application, which method would you choose and why?
   - What preprocessing steps might improve detection in challenging conditions?

---

## Troubleshooting Guide

### Common Issues

1. **Missing Model Files**

   - Some deep learning models need to be downloaded separately
   - Follow the instructions provided by `get_model_files()` function
   - Check file paths and permissions

2. **Poor Detection Results**

   - Adjust parameters (scale_factor, min_neighbors for Haar, confidence threshold for DNN)
   - Try image preprocessing (histogram equalization, normalization)
   - Consider image resolution (too small or too large can affect results)

3. **False Positives**

   - Increase confidence thresholds or min_neighbors
   - Apply additional validation (e.g., check for facial features within detected faces)
   - Use multiple detection passes with different parameters and take intersections

4. **Performance Issues**
   - Resize images before processing for faster detection
   - Use hardware acceleration if available (GPU, OpenCL)
   - Consider lighter models for real-time applications

### Debugging Tools

Create functions that:

1. Visualize detection confidence scores
2. Show step-by-step processing for DNN detection
3. Compare detection time and accuracy across methods

---

## Mini-Project: Face Detection Application

Create an application that:

1. Processes images from multiple sources (files, camera, or video)
2. Detects faces using the best method for each scenario
3. Tracks detection statistics (confidence, size, position)
4. Implements simple face recognition (optional, using face embeddings)
5. Saves annotated results and provides analysis

### Extension Ideas

- Add age and gender estimation
- Implement emotion detection from facial expressions
- Create a privacy tool that automatically blurs faces
- Build a face counting system for crowd analysis
- Develop a face-based UI control system (e.g., scrolling based on head position)

---

## Reflection Questions

1. What ethical considerations arise when implementing face detection systems?
2. How might biases in training data affect face detection accuracy across different demographics?
3. What advancements in deep learning have improved face detection in recent years?
4. How do face detection systems handle challenging cases like identical twins or face-altering makeup?
5. What privacy controls should be implemented in systems using face detection?
