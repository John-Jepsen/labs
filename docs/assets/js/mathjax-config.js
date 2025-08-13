// Advanced MathJax 3.x Configuration for Mathematical Documentation
// This configuration provides optimal rendering quality, performance, and accessibility

window.MathJax = {
  // Input processors
  tex: {
    // Enable advanced TeX packages and extensions
    packages: {
      '[+]': [
        'base',
        'autoload',
        'ams',
        'newcommand',
        'configMacros',
        'action',
        'bbox',
        'boldsymbol',
        'braket',
        'cancel',
        'color',
        'enclose',
        'extpfeil',
        'html',
        'mathtools',
        'mhchem',
        'physics',
        'textmacros',
        'unicode',
        'verb'
      ]
    },
    
    // Enhanced input delimiters
    inlineMath: [
      ['$', '$'],
      ['\\(', '\\)']
    ],
    displayMath: [
      ['$$', '$$'],
      ['\\[', '\\]']
    ],
    
    // Process escapes and environments
    processEscapes: true,
    processEnvironments: true,
    processRefs: true,
    
    // Custom macros for commonly used mathematical notation
    macros: {
      // Quantum Computing Notation
      'ket': ['\\left|#1\\right\\rangle', 1],
      'bra': ['\\left\\langle#1\\right|', 1],
      'braket': ['\\left\\langle#1\\middle|#2\\right\\rangle', 2],
      'ketbra': ['\\left|#1\\right\\rangle\\left\\langle#2\\right|', 2],
      'expval': ['\\left\\langle#1\\right\\rangle', 1],
      'op': ['\\hat{#1}', 1],
      'norm': ['\\left\\|#1\\right\\|', 1],
      
      // Linear Algebra
      'mat': ['\\mathbf{#1}', 1],
      'vec': ['\\mathbf{#1}', 1],
      'transpose': ['^{\\mathrm{T}}'],
      'hermitian': ['^{\\dagger}'],
      'inverse': ['^{-1}'],
      'trace': ['\\mathrm{tr}'],
      'rank': ['\\mathrm{rank}'],
      'span': ['\\mathrm{span}'],
      'nullspace': ['\\mathrm{null}'],
      
      // Calculus and Analysis
      'dd': ['\\,\\mathrm{d}#1', 1],
      'dv': ['\\frac{\\mathrm{d}#1}{\\mathrm{d}#2}', 2],
      'pdv': ['\\frac{\\partial#1}{\\partial#2}', 2],
      'gradient': ['\\nabla'],
      'divergence': ['\\nabla\\cdot'],
      'curl': ['\\nabla\\times'],
      'laplacian': ['\\nabla^2'],
      
      // Probability and Statistics
      'Pr': ['\\mathrm{Pr}'],
      'E': ['\\mathbb{E}'],
      'Var': ['\\mathrm{Var}'],
      'Cov': ['\\mathrm{Cov}'],
      'Corr': ['\\mathrm{Corr}'],
      'given': ['\\,|\\,'],
      'iid': ['\\stackrel{\\text{iid}}{\\sim}'],
      
      // Machine Learning
      'argmin': ['\\mathop{\\mathrm{argmin}}'],
      'argmax': ['\\mathop{\\mathrm{argmax}}'],
      'minimize': ['\\mathop{\\mathrm{minimize}}'],
      'maximize': ['\\mathop{\\mathrm{maximize}}'],
      'subjectto': ['\\mathop{\\mathrm{subject\\,to}}'],
      'sigmoid': ['\\sigma'],
      'softmax': ['\\mathrm{softmax}'],
      'relu': ['\\mathrm{ReLU}'],
      'loss': ['\\mathcal{L}'],
      
      // Set Theory and Logic
      'set': ['\\mathbb{#1}', 1],
      'R': ['\\mathbb{R}'],
      'C': ['\\mathbb{C}'],
      'N': ['\\mathbb{N}'],
      'Z': ['\\mathbb{Z}'],
      'Q': ['\\mathbb{Q}'],
      'emptyset': ['\\varnothing'],
      'powersetOf': ['\\mathcal{P}(#1)', 1],
      
      // Common Functions
      'abs': ['\\left|#1\\right|', 1],
      'floor': ['\\left\\lfloor#1\\right\\rfloor', 1],
      'ceil': ['\\left\\lceil#1\\right\\rceil', 1],
      'inner': ['\\left\\langle#1,#2\\right\\rangle', 2],
      
      // Complexity Theory
      'bigO': ['\\mathcal{O}(#1)', 1],
      'bigOmega': ['\\Omega(#1)', 1],
      'bigTheta': ['\\Theta(#1)', 1],
      'littleO': ['o(#1)', 1],
      
      // Custom environments for theorems, definitions, etc.
      'theorem': ['\\textbf{Theorem:}\\,'],
      'definition': ['\\textbf{Definition:}\\,'],
      'lemma': ['\\textbf{Lemma:}\\,'],
      'proposition': ['\\textbf{Proposition:}\\,'],
      'corollary': ['\\textbf{Corollary:}\\,'],
      'proof': ['\\textbf{Proof:}\\,'],
      'example': ['\\textbf{Example:}\\,'],
      'note': ['\\textbf{Note:}\\,'],
      'remark': ['\\textbf{Remark:}\\,']
    },
    
    // Tag configuration for equation numbering
    tags: 'ams',
    tagSide: 'right',
    tagIndent: '0.8em',
    
    // Error handling
    formatError: function (jax, err) {
      return jax.formatError(err);
    }
  },
  
  // Output configuration
  svg: {
    // High-quality SVG output
    fontCache: 'global',
    scale: 1,
    minScale: 0.5,
    matchFontHeight: false,
    mtextInheritFont: false,
    merrorInheritFont: false,
    mathmlSpacing: false,
    skipAttributes: {},
    exFactor: 0.5,
    displayAlign: 'center',
    displayIndent: '0',
    
    // Accessibility features
    internalSpeechTitles: true,
    titleID: 0
  },
  
  // Enhanced accessibility
  options: {
    enableMenu: true,
    menuOptions: {
      settings: {
        zoom: 'DoubleClick',
        CTRL: false,
        ALT: false,
        CMD: false,
        SHIFT: false
      }
    },
    
    // Performance optimization
    skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code'],
    includeHtmlTags: {
      br: '\n',
      wbr: '',
      '#comment': ''
    },
    
    // Responsive design
    showMathMenu: true,
    showMathMenuMSIE: true,
    
    // Processing options
    processHtmlClass: 'tex2jax_process',
    ignoreHtmlClass: 'tex2jax_ignore',
    processScriptType: 'math/tex',
    
    // Error display
    showProcessingMessages: false,
    messageStyle: 'none',
    
    // Document-ready processing
    typeset: true
  },
  
  // Startup configuration
  startup: {
    ready() {
      MathJax.startup.defaultReady();
      // Custom initialization
      console.log('Advanced MathJax configuration loaded successfully');
    },
    
    // Loading configuration
    pageReady() {
      return MathJax.startup.document.render();
    }
  },
  
  // Loader configuration for performance
  loader: {
    load: ['input/tex', 'output/svg', 'ui/menu', '[tex]/all-packages']
  }
};

// Fallback for when CDN is blocked - load MathJax even if main script fails
document.addEventListener('DOMContentLoaded', function() {
  // Check if MathJax loaded from CDN
  if (typeof window.MathJax === 'undefined' || !window.MathJax.startup) {
    console.log('MathJax CDN blocked, using fallback configuration');
    
    // Simple fallback to render basic math
    function renderFallbackMath() {
      const mathElements = document.querySelectorAll('script[type="math/tex"]');
      mathElements.forEach(element => {
        const tex = element.textContent;
        const span = document.createElement('span');
        span.className = 'math-fallback';
        span.textContent = tex;
        span.style.cssText = `
          font-family: 'Times New Roman', serif;
          font-style: italic;
          color: #2c3e50;
          background: rgba(64, 81, 181, 0.1);
          padding: 0.2em 0.4em;
          border-radius: 3px;
          margin: 0 0.2em;
        `;
        element.parentNode.replaceChild(span, element);
      });
      
      // Handle display math in $$ blocks
      const content = document.body.innerHTML;
      const displayMathRegex = /\$\$(.*?)\$\$/gs;
      const inlineMathRegex = /\$(.*?)\$/g;
      
      let newContent = content.replace(displayMathRegex, function(match, math) {
        return `<div class="math-display-fallback" style="
          text-align: center;
          margin: 1rem 0;
          padding: 1rem;
          background: linear-gradient(135deg, rgba(64, 81, 181, 0.1), rgba(64, 81, 181, 0.05));
          border-left: 4px solid rgba(64, 81, 181, 0.3);
          border-radius: 8px;
          font-family: 'Times New Roman', serif;
          font-style: italic;
          font-size: 1.1em;
          color: #2c3e50;
        ">${math.trim()}</div>`;
      });
      
      newContent = newContent.replace(inlineMathRegex, function(match, math) {
        return `<span class="math-inline-fallback" style="
          font-family: 'Times New Roman', serif;
          font-style: italic;
          color: #2c3e50;
          background: rgba(64, 81, 181, 0.1);
          padding: 0.1em 0.3em;
          border-radius: 3px;
          margin: 0 0.1em;
        ">${math.trim()}</span>`;
      });
      
      if (newContent !== content) {
        document.body.innerHTML = newContent;
      }
    }
    
    // Add a banner to indicate fallback mode
    const banner = document.createElement('div');
    banner.style.cssText = `
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      background: #ff9800;
      color: white;
      text-align: center;
      padding: 0.5rem;
      font-size: 0.9rem;
      z-index: 10000;
      box-shadow: 0 2px 4px rgba(0,0,0,0.2);
    `;
    banner.innerHTML = '⚠️ Mathematical notation is displayed in simplified format due to network restrictions';
    document.body.insertBefore(banner, document.body.firstChild);
    
    // Adjust body margin to account for banner
    document.body.style.marginTop = '3rem';
    
    setTimeout(renderFallbackMath, 100);
  }
  
  // Add copy-to-clipboard functionality for equations
  function addCopyToClipboard() {
    const mathElements = document.querySelectorAll('mjx-container, .math-fallback, .math-display-fallback, .math-inline-fallback');
    mathElements.forEach(element => {
      element.addEventListener('contextmenu', function(e) {
        e.preventDefault();
        
        // Get the TeX source
        const texSource = element.getAttribute('data-tex') || 
                         element.querySelector('[data-tex]')?.getAttribute('data-tex') ||
                         element.textContent ||
                         'Equation';
        
        // Copy to clipboard
        navigator.clipboard.writeText(texSource).then(() => {
          // Show feedback
          const feedback = document.createElement('div');
          feedback.textContent = 'Equation copied to clipboard!';
          feedback.style.cssText = `
            position: fixed;
            top: 20px;
            right: 20px;
            background: #4CAF50;
            color: white;
            padding: 10px 20px;
            border-radius: 5px;
            z-index: 10000;
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            box-shadow: 0 2px 10px rgba(0,0,0,0.2);
          `;
          document.body.appendChild(feedback);
          
          setTimeout(() => {
            feedback.remove();
          }, 2000);
        });
      });
    });
  }
  
  // Apply copy functionality after MathJax renders or fallback renders
  if (window.MathJax && window.MathJax.startup) {
    MathJax.startup.promise.then(() => {
      addCopyToClipboard();
    });
  } else {
    setTimeout(addCopyToClipboard, 500);
  }
});