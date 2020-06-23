mapboxgl.accessToken = "pk.eyJ1IjoiYm9ia3ViaW5lYyIsImEiOiJjazhyOTB4cjIwN2J4M2ZtczN4YW00MzAzIn0.Bl6FAUI-xnTqFwzqPK-BlQ";

const map = new mapboxgl.Map({
  attributionControl: false,
  container: "map",

  style: "mapbox://styles/bobkubinec/ckamp7q7j0ae01iqrfiivymat",
  center: [0, 38],
  maxZoom: 4,
  minZoom: 0,
  zoom: 1.9,
  bearing: 0,
  pitch: 0,
});

//map.addControl(new mapboxgl.FullscreenControl());
var nav = new mapboxgl.NavigationControl();
map.addControl(nav, "top-left");

map.addControl(
  new mapboxgl.AttributionControl({
    customAttribution: '© <a href="https://www.deepmoire.com/" target="_blank";">DeepMoiré</a>',
  })
);

// create popup
var popup = new mapboxgl.Popup({
  closeButton: false,
  closeOnClick: false,
  // anchor: 'bottom-left',
  // offset: [linearOffset, -linearOffset]
});

function numberWithCommas(x) {
  return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

// load map
map.on("load", function () {
  $(document).ready(function () {
    $.ajax({
      type: "GET",
	  url: "https://raw.githubusercontent.com/LuMesserschmidt/CoronaNet/master/docs/visual_js/data/data_viz_clean.csv",
      dataType: "text",
      success: function (csvData) {
        makeGeoJSON(csvData);
      },
    });
  });

  function onlyUnique(value, index, self) {
    return self.indexOf(value) === index;
  }

  function makeGeoJSON(csvData) {
    csv2geojson.csv2geojson(
      csvData,
      {
        latfield: "lat",
        lonfield: "lng",
        delimiter: ",",
      },
      function (err, data) {
        // //FIND ABSOULTE (LAST DAY) MAX VALUE
        // var listKeys = Object.keys(data.features[0].properties);

        // var daySplit = 53;

        // var listKeysDays = listKeys.splice(daySplit, listKeys.length);

        // lastDay = listKeys[listKeys.length - 1];
        // firstDay = listKeys[daySplit];

        // var casesMax = d3.max(data.features, function(d) {
        //   return parseInt(d.properties[lastDay]);
        // });

        map.addSource("points", {
          type: "geojson",
          data: data,
        });

        // map.addLayer({
        //   id: "points",
        //   type: "circle",
        //   source: "points",
        //   paint: {
        //     "circle-radius": 10,
        //     "circle-color": "#DD5145",
        //     "circle-opacity": 0.6
        //   }
        // });

        var sevIndexMin = d3.min(data.features, function (d) {
          return d.properties.severity_index;
        });

        var sevIndexMax = d3.max(data.features, function (d) {
          return d.properties.severity_index;
        });

        //console.log(sevIndexMin + "----" + sevIndexMax);

        map.addLayer({
          id: "heatmap",
          type: "heatmap",
          source: "points",
          paint: {
            "heatmap-weight": ["interpolate", ["linear"], ["to-number", ["get", "severity_index"]], 0, 0, 3, 1],
            "heatmap-intensity": 1,
            "heatmap-color": ["interpolate", ["linear"], ["heatmap-density"], 0, "rgba(0, 0, 255, 0)", 0.1, "hsl(28, 52%, 83%)", 1, "hsl(344, 86%, 46%)"],
            "heatmap-radius": 30,
            "heatmap-opacity": 0.8,
          },
          layout: {
            visibility: "none",
          },
        });

        //DOWNLOAD GEOJSON
        function download(content, fileName, contentType) {
          var a = document.createElement("a");
          var file = new Blob([content], { type: contentType });
          a.href = URL.createObjectURL(file);
          a.download = fileName;
          a.click();
        }
        var dictstring = JSON.stringify(data);

        function filterBy(value) {
          //var filters = ["all", ["<", "TimeCodeReduced", step], ["==", "month", month]];
        }

        d3.json("https://raw.githubusercontent.com/LuMesserschmidt/CoronaNet/master/docs/visual_js/data/toMapbox-02.geojson", function (err, data1) {
          if (err) throw err;

          map.addSource("worldCountries", {
            type: "geojson",
            data: data1,
          });

          map.addSource("worldCountries2", {
            type: "geojson",
            data: data1,
          });

          map.addLayer(
            {
              id: "worldCountries",
              type: "fill",
              source: "worldCountries",
              layout: {},
              paint: {
                "fill-color": "#848C94",
                "fill-opacity": ["case", ["boolean", ["feature-state", "hover"], false], 0.8, 0.001],
              },
            },
            "poi-label"
          );

          map.addLayer(
            {
              id: "worldCountries2",
              type: "fill",
              source: "worldCountries2",
              layout: {},
              paint: {
                "fill-color": "#da1046",
                "fill-opacity": ["case", ["boolean", ["feature-state", "hover"], false], 0.8, 0.001],
              },
            },
            "poi-label"
          );

          // loading indicator
          map.on("data", function (e) {
            if (e.dataType === "source" && e.sourceId === "worldCountries2") {
              document.getElementById("loader").style.visibility = "hidden";
            }
          });

          //TRANSFORM DATA STRINGS TO REAL DATA AND PREPARE IT FOR FILTER BY SLIDER
          data.features = data.features.map(function (d) {
            var clean = d.properties.date_announced.replace(/-/g, "/");
			var dateParts = clean.split("/");
			d.properties.dateLong = new Date(+dateParts[2], dateParts[1] - 1, +dateParts[0]);
			d.properties.dateShort = new Date(+dateParts[2], dateParts[1] - 1, +dateParts[0]).toLocaleDateString("en-US");
			d.properties.test = Date.parse(d.properties.dateLong);

            return d;
          });

          var timeStamp = data.features.map(function (d) {
            return d.properties.test;
          });

          timeStamp = timeStamp.filter(onlyUnique).sort();

          timeStamp = timeStamp.filter(function (d) {
            return d !== "Invalid Date";
          });

          d3.select("#slider").property("max", timeStamp.length - 1);
          d3.select("#slider").property("value", timeStamp.length - 1);

          slider.addEventListener("input", function (e) {
			  highlightCountries(currentCountry);
		  });

          // highlight countries on hover
          var hoveredStateId = null;
		  var currentCountry = null;
		  
		  map.on("mousemove", "worldCountries", function(e) {
			  if (e.features.length > 0) {
				  if (hoveredStateId) {
					  if (currentCountry !== null) {
						  map.setFeatureState({ source: "worldCountries", id: hoveredStateId }, { hover: hoveredStateId === currentCountry.id });
					  } else {
						  map.setFeatureState({ source: "worldCountries", id: hoveredStateId }, { hover: false });
					  }
					
				  }
			  }
				hoveredStateId = e.features[0].id;
				var coords = e.lngLat;
				var infoShow = ["<div class='labelCountry'>" + e.features[0].properties.ADMIN + "</div>"];
				popup.setLngLat(coords).setHTML(infoShow).addTo(map);
				map.setFeatureState({ source: "worldCountries", id: hoveredStateId }, { hover: true });
		  });
		  
		  map.on("mouseleave", "worldCountries", function(e) {
			  if (hoveredStateId) {
				if (currentCountry !== null && currentCountry.id !== null) {
					map.setFeatureState({ source: "worldCountries", id: hoveredStateId }, { hover: hoveredStateId === currentCountry.id });
				} else {
					map.setFeatureState({ source: "worldCountries", id: hoveredStateId }, { hover: false });
				}
			  }
			hoveredStateId = null;
			
		  });

          map.on("click", "worldCountries", function(e) {highlightCountries(e.features[0])});
		  
		  function highlightCountries(sourceCountry) {
            var sliderVal = document.getElementById("slider").value;
            var humanDate = new Date(timeStamp[sliderVal]).toLocaleDateString("en-US");

            document.getElementById("slider-label").innerHTML = "Responses up to " + humanDate;

            dataFiltered = data.features.filter(function (d) {
              return d.properties.test <= timeStamp[sliderVal];
            });

            map.getCanvas().style.cursor = "pointer";

            var totalIndex = data1.features.map(function (d) {
              return d.id;
            });

            totalIndex.forEach(function (item, index) {
              map.setFeatureState({ source: "worldCountries2", id: item }, { hover: false });
            });

			if (sourceCountry !== null) {
			  if (currentCountry !== null && currentCountry.id !== null) {
                map.setFeatureState({ source: "worldCountries", id: currentCountry.id }, { hover: false });
              }
			  hoveredStateId = sourceCountry.id;
              map.setFeatureState({ source: "worldCountries", id: hoveredStateId }, { hover: true });
			  currentCountry = sourceCountry;
            }

			var callP = sourceCountry.properties;

            var filterCountryObj = dataFiltered.filter(function (d) {
              return d.properties.country == callP.ADMIN;
            });

            var countryTitle = "<div class='labelTitle'>Country</div><div class='labelTitle-1'>" + callP.ADMIN + "</div>";

            if (filterCountryObj.length == 0) {
              document.getElementById("text-panel").innerHTML = countryTitle + "<div class='labelAlert'>No Responses so far</div>";
            }

            var sorted = filterCountryObj.sort(function (a, b) {
              return parseFloat(a.properties.test) - parseFloat(b.properties.test);
            });

            var joinedReport = [];
            for (index = 0; index < [sorted.length - 1]; index++) {
              var callP3 = sorted[index].properties;

              var keysList = Object.keys(callP3);
              keysListFilt = [];
              indexArr = [8, 1, 3, 7, 0];
              var titles = ["Date", "Status", "Type", "Compliance", "Record ID"];

              for (var i = 0; i < indexArr.length; i++) keysListFilt.push(keysList[indexArr[i]]);

              var infoShow = [];

              keysListFilt.forEach(function (item, i) {
                var htmlLine = "<div class='label1'>" + titles[i] + "</div>" + "<div class='label2'>" + callP3[item] + "</div>";
                infoShow.push(htmlLine);
              });

              var targetWithSpaces = callP3.target_country.split(",").join(", ");

              var target = "<div class='label1'>Target</div>" + "<div id='wrap-container'><div class='label2'>" + targetWithSpaces + "</div></div>";
              var linkSource = "<div class='label1'>Link</div><div class='label3'><a href='" + callP3.link + "' target='_blank'>Visit the source</a></div>";

              var infoShow = infoShow.join("<p>");

              joinedReport.push(infoShow + target + linkSource);
            }

            joinedReport.reverse();

            document.getElementById("text-panel").innerHTML = countryTitle + joinedReport.join("--------------------------------");

            var targetCounties = filterCountryObj.map(function (d) {
              return d.properties.target_country;
            });

            targetCounties = targetCounties.join();
            targetCounties = targetCounties.split(",");

            targetCountiesUniq = targetCounties.filter(onlyUnique);

            targetCountiesUniq = targetCountiesUniq.filter(function (el) {
              return el !== "";
            });

            var indexDomestic = targetCountiesUniq.indexOf("Domestic");

            if (indexDomestic !== -1) {
              targetCountiesUniq[indexDomestic] = callP.ADMIN;
            }

            targetCountiesUniq = targetCountiesUniq.filter(function (el) {
              return el !== callP.ADMIN;
            });

            targetCountiesUniq = targetCountiesUniq.filter(onlyUnique);

            targetCountiesUniq2 = targetCountiesUniq.filter(function (el) {
              return el == "All";
            });

            var indexDomestic = data1.features.filter(function (d) {
              return d.properties.ADMIN == callP.ADMIN;
            });

            indexDomestic = indexDomestic[0].id;

            var indicesTarget = [];
            targetCountiesUniq.forEach(function (item, index) {
              var filterIndexGeo = data1.features.filter(function (d) {
                return d.properties.ADMIN == item;
              });
              var IndexGeo = filterIndexGeo.map(function (d) {
                return d.id;
              });
              indicesTarget.push(IndexGeo);
            });

            indicesTarget = [].concat.apply([], indicesTarget);

            var isoA3code = data1.features.map(function (d) {
              return d.properties.ISO_A3;
            });

            if (targetCountiesUniq2.length == 0) {
              indicesTarget.forEach(function (item, index) {
                map.setFeatureState({ source: "worldCountries2", id: item }, { hover: true });
              });
            } else {
              for (index = 0; index < 300; index++) {
                if (index != indexDomestic) {
                  map.setFeatureState({ source: "worldCountries2", id: index }, { hover: true });
                } else {
                }
              }
            }
			
			if (document.getElementsByClassName("label1").length === 0) {
				document.getElementById("text-panel").innerHTML = countryTitle + "<div class='labelAlert'>No Responses so far</div>";
			}
          }

          map.on("mouseleave", "worldCountries", function () {
            popup.remove();
          });

          map.on("mouseenter", "worldCountries", function (e) {
            map.getCanvas().style.cursor = "pointer";
          });

          map.on("click", "water", function (e) {
            document.getElementById("text-panel").innerHTML = '<div class="labelClick">Click the map to see a country\'s policies and the targets of those policies</div>';
            if (e.defaultPrevented === false) {
              popup.remove();

              for (index = 0; index < 300; index++) {
                map.setFeatureState({ source: "worldCountries", id: index }, { hover: false });
                map.setFeatureState({ source: "worldCountries2", id: index }, { hover: false });
              }
			  currentCountry = null;
            }
          });
        });

        $("#viz1").on("click", function (e) {
          e.preventDefault();
          this.className = "active";
          document.getElementById("viz2").className = "";
          //map.setLayoutProperty("worldCountries", "visibility", "visible");
          map.setLayoutProperty("worldCountries2", "visibility", "visible");
          map.setLayoutProperty("heatmap", "visibility", "none");
        });

        $("#viz2").on("click", function (e) {
          e.preventDefault();
          this.className = "active";
          document.getElementById("viz1").className = "";
          //map.setLayoutProperty("worldCountries", "visibility", "none");
          map.setLayoutProperty("worldCountries2", "visibility", "none");
          map.setLayoutProperty("heatmap", "visibility", "visible");
        });
      }
    );
  }
});

//PUT THE PANEL DOWN WHEN CLICKING THE DOUBLE-ARROW ICON

if ($(window).width() < 500) {
  document.getElementById("arrow-mobile").innerHTML = "▼";

  document.getElementById("text-panel").innerHTML = '<div class="label1">CLICK A COUNTRY ON THE MAP</div>';

  $("#arrow-mobile").click(function (e) {
    e.preventDefault();
    if (this.className == "active") {
      this.className = "";
      document.getElementById("arrow-mobile").innerHTML = "▼";
      document.getElementById("mobile").style.top = "65%";
    } else {
      this.className = "active";
      document.getElementById("arrow-mobile").innerHTML = "▲";
      document.getElementById("mobile").style.top = "90%";
    }
  });
} else {
  document.getElementById("text-panel").innerHTML = '<div class="labelClick">Click the map to see a country\'s policies and the targets of those policies.</div>';
  document.getElementById("arrow-mobile").innerHTML = "◄";

  $("#arrow-mobile").click(function (e) {
    e.preventDefault();
    if (this.className == "active") {
      this.className = "";
      document.getElementById("arrow-mobile").innerHTML = "◄";
      document.getElementById("mobile").style.left = "10px";
    } else {
      this.className = "active";
      document.getElementById("arrow-mobile").innerHTML = "►";
      document.getElementById("mobile").style.left = "-340px";
    }
  });
}
