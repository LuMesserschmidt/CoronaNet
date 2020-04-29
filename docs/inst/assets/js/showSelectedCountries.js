function loadCountries(el, x) {
  // retrieve the instance of the dygraph
  var d = HTMLWidgets.getInstance(document.getElementById('policyActivityIndexDygraph')).dygraph;
  //var d = this.dygraph;
  // exclude date_announced
  var country_names = d.getLabels().slice(1);
  $(document).ready(function() {
      $('#countrySearch').select2({dropdownAutoWidth: true,
      width: '100%',
      closeOnSelect: false,
      // include clear all selections button
      allowClear: true,
      // placeholder is necessary for allowClear to work
      placeholder: 'Search for a country'});
  });

  var searchBar = document.getElementById('countrySearch');
	
  // add each country in dataset to dropdown
  for (var i = 0; i < country_names.length; i++) {
    var option = document.createElement('option');
    option.text = country_names[i];
    option.value = i;
    searchBar.add(option, i);
  }
	
  // change data on select
  $('#countrySearch').on('select2:select', function (e) {
    var countryId = e.params.data.id;
    // if previously all displayed, show only the selected value
	if (d.visibility().every(value => value === true)) {
    	d.visibility().fill(false);
    }
    d.visibility()[countryId] = true;
    // dummy update to reload data
    d.updateOptions({'showRangeSelector': false});
  });

	// change data on deselect
  $('#countrySearch').on('select2:unselect', function (e) {
    var countryId = e.params.data.id;
    d.visibility()[countryId] = false;
    // if all values deselected, show all countries
	if (d.visibility().every(value => value === false)) {
    	d.visibility().fill(true);
    }
    // dummy update to reload data
    d.updateOptions({'showRangeSelector': false});
  });
}
