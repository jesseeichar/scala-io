function VersionIndexCtrl($xhr) {
  var self = this;
  this.versions = $xhr('GET', 'version-index.json', function(code, response) {
        self.versions = response;
      });
}