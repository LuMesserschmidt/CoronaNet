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

// test of new functionality
function loadCountriesWithColors(el, x) {
  // retrieve the instance of the dygraph
  var d = HTMLWidgets.getInstance(document.getElementById('policyActivityIndexDygraph')).dygraph;
  var activeColor = 'darkred';
  var inactiveColor = '#A0A0A0';
  // exclude date_announced
  var country_names = d.getLabels().slice(1);
  var colorStorage = d.colorsMap_;
  $(document).ready(function() {
      $('#countrySearch').select2({dropdownAutoWidth: true,
      width: '100%',
      closeOnSelect: false,
      // include clear all selections button
      allowClear: true,
      // placeholder is necessary for allowClear to work
      placeholder: 'Search for a country'});
  });
  
  function updateColors(e, x, points) {
	var selectedCountry = d.getHighlightSeries();
	if (colorStorage[selectedCountry] === inactiveColor) {
		d.colorsMap_[selectedCountry] = activeColor;
		colorStorage[selectedCountry] = activeColor;
	} else {
		d.colorsMap_[selectedCountry] = inactiveColor;
		colorStorage[selectedCountry] = inactiveColor;
	}
	var arr = d.attributes_.series_;
	Object.keys(arr).forEach(function(key) {
		var countryIndex = arr[key].idx;
		if (d.visibility()[countryIndex]) {
			d.colorsMap_[key] = colorStorage[key];
			d.user_attrs_.colors[countryIndex] = colorStorage[key];
		}
	})
	Dygraph.updateDeep(d.user_attrs_, d.user_attrs_);
	// dummy update to reload data
	d.updateOptions({});
  }
  
  d.updateOptions({'clickCallback': updateColors});

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
	var arr = d.attributes_.series_;
	Object.keys(arr).forEach(function(key) {
		var countryIndex = arr[key].idx;
		if (d.visibility()[countryIndex]) {
			d.colorsMap_[key] = colorStorage[key];
			d.user_attrs_.colors[countryIndex] = colorStorage[key];
		}
	})
	Dygraph.updateDeep(d.user_attrs_, d.user_attrs_);
    d.updateOptions({'showRangeSelector': false});
  });
}