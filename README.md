# Interactive Documentation

This project enhances existing markdown documentation with interactive features using Material for MkDocs. The system provides:

- Interactive code examples
- Search functionality
- Syntax highlighting
- Responsive design
- Offline capabilities
- Student exercises and challenges

## Setup

1. Install the required dependencies:

```bash
pip install -r requirements.txt
```

2. Run the setup script to copy and enhance your markdown files:

```bash
python setup_markdown.py
```

3. Start the development server:

```bash
mkdocs serve
```

4. Open your browser to `http://127.0.0.1:8000`

## Features

- **Interactive Code Examples**: Run and modify code directly in the browser
- **Search**: Find content quickly with the built-in search functionality
- **Syntax Highlighting**: Code examples are properly formatted and highlighted
- **Responsive Design**: Works on desktop and mobile devices
- **Offline Mode**: Documentation can be downloaded and used offline
- **Student Exercises**: Each topic includes practice problems and challenges

## Structure

The documentation maintains your existing structure while adding interactive elements:

- Code blocks become executable
- Practice problems are added to each page
- Challenges are included for advanced topics
- Search functionality works across all content

## Customization

You can customize the interactive elements by:

1. Modifying the CSS in `docs/assets/css/interactive.css`
2. Adjusting the JavaScript in `docs/assets/js/interactive.js`
3. Updating the MkDocs configuration in `mkdocs.yml`

## License

This project is licensed under the MIT License - see the LICENSE file for details.
