DocsController.$inject = ['$location', '$browser', '$window', '$cookies'];
function DocsController($location, $browser, $window, $cookies) {
    this.props = IO_PROPS;
  window.$root = this.$root;
  var self = this,
      OFFLINE_COOKIE_NAME = 'ng-offline',
      HAS_HASH = /#/;
  
  this.$location = $location;
  if (!HAS_HASH.test($location.href)) {
      $location.hashPath = '!/overview';
  }
  this.$watch('$location.hashPath', function(scope,hashPath) {
    if (hashPath.match(/^!/)) {
      var parts = hashPath.substring(1).split('/');
      self.sectionId = parts[1];
      self.partialId = parts[2] || 'index';
      if(self.sectionId === 'file' || self.sectionId === 'core' || self.sectionId === 'performance') {
        self.pages = angular.Array.filter(IO_PAGES, {section:self.sectionId});
      } else {
          self.pages = IO_PAGES;
      }

      var i = self.pages.length;
      while (i--) {
        if (self.pages[i].id == self.partialId && self.pages[i].section == self.sectionId) {
          self.partialTitle = self.pages[i].name
          break;
        }
      }
      if (i<0) {
        self.partialTitle = 'Error: Page Not Found!';
        delete self.partialId;
      }
    }
  });

  this.getUrl = function(page){
    return '#!/' + page.section + '/' + page.id;
  };

  this.getCurrentPartial = function(){
    return this.partialId ? ('./' + this.sectionId + '/' + this.partialId + '.html') : '';
  };

  this.getClass = function(page) {
    var depth = page.depth,
        cssClass = 'level-' + depth + (page.name == this.partialId ? ' selected' : '');

    if (page.section == 'api')
      cssClass += ' monospace';

    return cssClass;
  };

  this.selectedSection = function(section) {
    return section == self.sectionId ? 'current' : '';
  };

  this.selectedPartial = function(page) {
    return (page.section == self.sectionId && page.id == self.partialId) ? 'current' : '';
  };

  this.afterPartialLoaded = function() {
    SyntaxHighlighter.highlight();
    $window.scrollTo(0,0);
  };

  this.getFeedbackUrl = function() {
    return "mailto:angular@googlegroups.com?" +
           "subject=" + escape("Feedback on " + $location.href) + "&" +
           "body=" + escape("Hi there,\n\nI read " + $location.href + " and wanted to ask ....");
  };

}