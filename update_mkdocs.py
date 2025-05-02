#!/usr/bin/env python3

import os
import sys
import yaml
import subprocess
from pathlib import Path
import logging
from collections import OrderedDict

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

class MkDocsUpdater:
    def __init__(self, docs_dir='docs', config_file='mkdocs.yml'):
        self.docs_dir = Path(docs_dir)
        self.config_file = Path(config_file)
        self.nav_changes = {
            'added': [],
            'removed': [],
            'updated': []
        }
        
        # Ensure the docs directory exists
        if not self.docs_dir.exists():
            logger.error(f"Docs directory '{docs_dir}' not found!")
            sys.exit(1)
        
        # Check if mkdocs.yml exists
        if not self.config_file.exists():
            logger.error(f"MkDocs config file '{config_file}' not found!")
            sys.exit(1)
        
        # Load the current config
        with open(self.config_file, 'r') as f:
            self.config = yaml.safe_load(f) or {}
            if not isinstance(self.config, dict):
                self.config = {}
                
    def scan_docs_directory(self):
        """Scan the docs directory and build a navigation structure."""
        logger.info(f"Scanning docs directory: {self.docs_dir}")
        
        nav_structure = {}
        
        # Get all markdown files
        for md_file in self.docs_dir.glob('**/*.md'):
            # Skip hidden directories
            if any(part.startswith('.') for part in md_file.parts):
                continue
                
            relative_path = md_file.relative_to(self.docs_dir)
            path_parts = list(relative_path.parts)
            
            # Process the path to create a hierarchical structure
            current_level = nav_structure
            
            # Handle all directory levels
            for i, part in enumerate(path_parts[:-1]):
                if part not in current_level:
                    current_level[part] = {}
                current_level = current_level[part]
            
            # Handle the file level
            filename = path_parts[-1]
            filepath = str(relative_path).replace('\\', '/')
            
            # Special handling for index.md files
            if filename == 'index.md':
                # If this is a top-level index.md, store it specially
                if len(path_parts) == 1:
                    nav_structure['index'] = filepath
                else:
                    # For index.md in subdirectories, store as the directory's value
                    parent_dir = path_parts[-2]
                    if isinstance(current_level.get(parent_dir), dict):
                        current_level[parent_dir]['index'] = filepath
                    else:
                        current_level['index'] = filepath
            else:
                # For non-index files, use the filename without extension as the key
                file_key = filename.rsplit('.', 1)[0]
                current_level[file_key] = filepath
        
        return nav_structure
                
    def convert_structure_to_nav(self, structure, current_level=None):
        """Convert the hierarchical structure to MkDocs nav format."""
        if current_level is None:
            current_level = []
            
        nav_items = []
        
        # Handle direct 'index' entry first if it exists
        if 'index' in structure:
            # Index at root level gets special treatment
            if not current_level:
                nav_items.append({'Home': structure['index']})
            else:
                # For subdirectories, use the current directory name
                dir_name = current_level[-1].title() if current_level else 'Overview'
                nav_items.append({dir_name: structure['index']})
            
            # Remove index so we don't process it again
            structure = {k: v for k, v in structure.items() if k != 'index'}
        
        # Sort the keys alphabetically for consistent ordering
        sorted_keys = sorted(structure.keys())
        
        for key in sorted_keys:
            value = structure[key]
            
            if isinstance(value, dict):
                # It's a directory
                new_level = current_level + [key]
                
                # Check if it has an index
                title = key.replace('-', ' ').replace('_', ' ').title()
                
                # Recursively process the directory
                child_items = self.convert_structure_to_nav(value, new_level)
                
                # If child_items is a list with a single item and that item is a dict
                # with a single entry where the key is the title we want, simplify
                if isinstance(child_items, list) and len(child_items) == 1 and isinstance(child_items[0], dict):
                    if list(child_items[0].keys())[0] == title and list(child_items[0].values())[0] == value.get('index'):
                        # Use simple format: "Title: path"
                        nav_items.append({title: value.get('index')})
                    else:
                        # Use nested format: "Title: [items]"
                        nav_items.append({title: child_items})
                else:
                    # Use nested format: "Title: [items]"
                    nav_items.append({title: child_items})
            else:
                # It's a file
                title = key.replace('-', ' ').replace('_', ' ').title()
                nav_items.append({title: value})
        
        return nav_items
    
    def update_config_nav(self, new_nav):
        """Update the nav section of the MkDocs config."""
        logger.info("Updating mkdocs.yml navigation")
        
        # Get current nav if it exists
        current_nav = self.config.get('nav', [])
        
        # Compare and track changes
        current_entries = self._collect_nav_entries(current_nav)
        new_entries = self._collect_nav_entries(new_nav)
        
        self.nav_changes['added'] = [path for path in new_entries if path not in current_entries]
        self.nav_changes['removed'] = [path for path in current_entries if path not in new_entries]
        
        # Update the config
        self.config['nav'] = new_nav
        
        # Save the updated config
        with open(self.config_file, 'w') as f:
            yaml.dump(self.config, f, default_flow_style=False, sort_keys=False)
    
    def _collect_nav_entries(self, nav, paths=None):
        """Collect all markdown file paths from a nav structure."""
        if paths is None:
            paths = set()
            
        if isinstance(nav, list):
            for item in nav:
                self._collect_nav_entries(item, paths)
        elif isinstance(nav, dict):
            for key, value in nav.items():
                if isinstance(value, str) and value.endswith('.md'):
                    paths.add(value)
                else:
                    self._collect_nav_entries(value, paths)
                    
        return paths
    
    def build_site(self):
        """Build the MkDocs site."""
        logger.info("Building MkDocs site")
        
        try:
            result = subprocess.run(
                ['mkdocs', 'build', '--clean'],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=True
            )
            logger.info("Site built successfully")
            logger.debug(f"Build output: {result.stdout}")
            return True
        except subprocess.CalledProcessError as e:
            logger.error(f"Error building site: {e}")
            logger.error(f"Command output: {e.stderr}")
            return False
    
    def run(self):
        """Run the full update process."""
        # Scan docs directory
        nav_structure = self.scan_docs_directory()
        
        # Convert to nav format
        new_nav = self.convert_structure_to_nav(nav_structure)
        
        # Update config
        self.update_config_nav(new_nav)
        
        # Build the site
        build_success = self.build_site()
        
        # Report changes
        self._report_changes()
        
        return build_success
    
    def _report_changes(self):
        """Report changes made to the navigation."""
        if self.nav_changes['added']:
            logger.info(f"Added {len(self.nav_changes['added'])} items to navigation:")
            for item in sorted(self.nav_changes['added']):
                logger.info(f"  + {item}")
                
        if self.nav_changes['removed']:
            logger.info(f"Removed {len(self.nav_changes['removed'])} items from navigation:")
            for item in sorted(self.nav_changes['removed']):
                logger.info(f"  - {item}")
                
        if not self.nav_changes['added'] and not self.nav_changes['removed']:
            logger.info("No changes to navigation structure")

if __name__ == '__main__':
    logger.info("Starting MkDocs update process")
    
    updater = MkDocsUpdater()
    success = updater.run()
    
    if success:
        logger.info("MkDocs update completed successfully")
    else:
        logger.error("MkDocs update failed!")
        sys.exit(1) 