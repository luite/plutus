/*global exports, require*/
'use strict';

var Chartist = require('chartist');

exports.sampleAxisTitleOptions = {
    axisX: {
        axisTitle: 'Time (mins)',
        axisClass: 'ct-axis-title',
        offset: {
            x: 0,
            y: 40
        },
        textAnchor: 'middle'
    },
    axisY: {
        axisTitle: 'Goals',
        axisClass: 'ct-axis-title',
        offset: {
            x: 0,
            y: -30
        },
        textAnchor: 'middle',
        flipTitle: false
    }
};

exports.samplePlugins = [
    Chartist.plugins.tooltip(),
    Chartist.plugins.ctAxisTitle(exports.sampleAxisTitleOptions)
];

exports.sampleResponsiveOptions = [
    ['screen and (max-width: 640px)', {
        seriesBarDistance: 5,
        axisX: {
            labelInterpolationFnc: function (value) {
                return value[0];
            }
        }
    }]
];

exports._updateData = function (chart, newData) {
    chart.update(newData);
};

exports._barChart = function (element, options, responsiveOptions) {
    return new Chartist.Bar(
        element,
        {},
        options,
        responsiveOptions
    );
};
