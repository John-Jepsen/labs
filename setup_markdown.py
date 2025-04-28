import os
import shutil
from pathlib import Path
import yaml

def copy_markdown_files(source_dir, target_dir):
    """Copy markdown files from source to target directory."""
    source_path = Path(source_dir)
    target_path = Path(target_dir)
    
    # Create target directory if it doesn't exist
    target_path.mkdir(parents=True, exist_ok=True)
    
    # Copy all markdown files
    for md_file in source_path.rglob('*.md'):
        # Skip files in .chat_history directory
        if '.chat_history' in str(md_file):
            continue
            
        relative_path = md_file.relative_to(source_path)
        target_file = target_path / relative_path
        
        # Create parent directories if they don't exist
        target_file.parent.mkdir(parents=True, exist_ok=True)
        
        # Copy the file
        shutil.copy2(md_file, target_file)
        print(f"Copied {md_file} to {target_file}")

def create_mkdocs_config():
    """Create the mkdocs.yml configuration file."""
    config = {
        'site_name': 'Collaborative Documentation',
        'theme': {
            'name': 'material',
            'features': [
                'navigation.tabs',
                'navigation.sections',
                'navigation.expand',
                'navigation.top',
                'search.highlight',
                'search.suggest',
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
            'pymdownx.superfences',
            'pymdownx.tabbed',
            'pymdownx.tasklist',
        ],
        'plugins': [
            'search',
            'minify'
        ]
    }
    
    with open('mkdocs.yml', 'w') as f:
        yaml.dump(config, f, default_flow_style=False)

def setup_docs(source_dir='../collab'):
    """Set up the documentation with existing markdown files."""
    # Create necessary directories
    docs_dir = Path('docs')
    docs_dir.mkdir(exist_ok=True)
    
    # Copy markdown files
    print("Copying markdown files...")
    copy_markdown_files(source_dir, docs_dir)
    
    # Create mkdocs.yml
    print("Creating MkDocs configuration...")
    create_mkdocs_config()
    
    # Create requirements.txt
    with open('requirements.txt', 'w') as f:
        f.write('''mkdocs>=1.5.0
mkdocs-material>=9.0.0
pyyaml>=6.0.0
mkdocs-minify-plugin>=0.6.0
''')

if __name__ == '__main__':
    setup_docs() 