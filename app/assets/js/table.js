var app = app || {};

app.table = function(){

    var init = function() {
        initEventHandlers();
    };

    var initEventHandlers = function() {
        initRowDetails();
    };

    var initRowDetails = function() {

        $('.table').on('click', '.rowdetails td:first-child', function(event) {

            var row = $(event.target).closest('.rowdetails');
            var details = row.find('.rowdetails-content');

            var rowHeight = row.closest('table').find('tr').height();

            if(!row.hasClass('expanded')) {

                details[0]._originalHeight = details[0]._originalHeight || details.height();
                details.height(0);

                row.animate({
                    height: rowHeight + details[0]._originalHeight + 20
                });

                details.show().animate({
                    height: details[0]._originalHeight,
                    opacity: 1
                });
            } else {

                row.animate({
                    height: rowHeight
                });

                details.animate({
                    height: 0,
                    opacity: 0
                });
            }

            row.toggleClass('expanded');
        });
    };

    return {
        init: init
    };


}();

app.register(app.table);
