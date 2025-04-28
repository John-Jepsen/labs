import os
import shutil
from pathlib import Path
import mammoth
import yaml
import re
from enhance_markdown import process_all_markdown

def convert_docx_to_markdown(docx_path, output_dir):
    """Convert a DOCX file to markdown format."""
    with open(docx_path, 'rb') as docx_file:
        result = mammoth.convert_to_markdown(docx_file)
        markdown = result.value
        
        # Clean up the markdown
        markdown = re.sub(r'\n{3,}', '\n\n', markdown)  # Remove excessive newlines
        markdown = markdown.strip()
        
        # Create output filename
        filename = os.path.basename(docx_path).replace('.docx', '.md')
        output_path = os.path.join(output_dir, filename)
        
        with open(output_path, 'w', encoding='utf-8') as md_file:
            md_file.write(markdown)
        
        return output_path

def create_mkdocs_config():
    """Create the mkdocs.yml configuration file."""
    config = {
        'site_name': 'Interactive Algorithm Documentation',
        'theme': {
            'name': 'material',
            'features': [
                'navigation.tabs',
                'navigation.sections',
                'navigation.expand',
                'navigation.top',
                'search.highlight',
                'search.suggest',
                'search.share',
            ],
            'palette': {
                'scheme': 'default',
                'primary': 'indigo',
                'accent': 'indigo'
            }
        },
        'markdown_extensions': [
            'admonition',
            'codehilite',
            'footnotes',
            'meta',
            'toc',
            'pymdownx.arithmatex',
            'pymdownx.betterem',
            'pymdownx.caret',
            'pymdownx.critic',
            'pymdownx.details',
            'pymdownx.emoji',
            'pymdownx.inlinehilite',
            'pymdownx.keys',
            'pymdownx.mark',
            'pymdownx.smartsymbols',
            'pymdownx.superfences',
            'pymdownx.tabbed',
            'pymdownx.tasklist',
            'pymdownx.tilde'
        ],
        'plugins': [
            'search',
            'minify'
        ]
    }
    
    with open('mkdocs.yml', 'w') as f:
        yaml.dump(config, f, default_flow_style=False)

def setup_docs():
    # Create necessary directories
    docs_dir = Path('docs')
    docs_dir.mkdir(exist_ok=True)
    
    # Convert all DOCX files to markdown
    source_dir = Path('../lab_docs')
    for docx_file in source_dir.glob('*.docx'):
        print(f"Converting {docx_file.name}...")
        convert_docx_to_markdown(docx_file, docs_dir)
    
    # Enhance markdown files with interactive elements
    print("Adding interactive elements...")
    process_all_markdown()
    
    # Create mkdocs.yml
    create_mkdocs_config()
    
    # Create requirements.txt
    with open('requirements.txt', 'w') as f:
        f.write('''mkdocs>=1.5.0
mkdocs-material>=9.0.0
mammoth>=1.6.0
pyyaml>=6.0.0
''')

if __name__ == '__main__':
    setup_docs() 