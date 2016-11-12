
'use strict';

var app = {}; // create namespace for our app


app.log_append = function(entry) {
  var log_area = $("#disciple-log");
  log_area.val(log_area.val() + "\n" + entry);
};

app.do_disciple_action = function(action, content, view, model) {
  var encoded_content = JSON.stringify(content);
  $.ajax({
    type: "POST",
    url: model.url() + "/" + action,
    data: encoded_content,
    success: function(data, code, xhr) {
      app.log_append(JSON.stringify(data));
      model.set(data);
      view.render();
    },
    dataType: "json",
    contentType: "application/json; charset=UTF-8"
  });
};

//--------------
// Models
//--------------

app.Disciple = Backbone.Model.extend({
  defaults: {
    name: "Bob",
    skill: 100,
    health: 100,
    confidence: 100,
    pride: 100,
    focus: 100,
    discipline: 100
  }
});

//--------------
// Collections
//--------------
app.DiscipleList = Backbone.Collection.extend({
  model: app.Disciple,
  url: '/1/disciple',
  parse: function(response) {
    return response.disciples;
  }
});

app.discipleList = new app.DiscipleList();

//--------------
// Views
//--------------


app.DiscipleView = Backbone.View.extend({
  tagName: 'li',
  template: _.template($('#disciple-template').html()),
  render: function(){
    this.$el.html(this.template(this.model.toJSON()));
    return this; // enable chained calls
  },
  events: {
    'click .encourage': 'encourageDisciple',
    'click .chastise': 'chastiseDisciple',
    'click .refocus': 'refocusDisciple'
  },
  encourageDisciple: function(e) {
    var encourage_content = { feedback_type: "encourage" };
    app.do_disciple_action("feedback", encourage_content, this, this.model);
  },
  chastiseDisciple: function(e) {
    var chastise_content = { feedback_type: "chastise" };
    app.do_disciple_action("feedback", chastise_content, this, this.model);
  },
  refocusDisciple: function(e) {
    var refocus_content = { feedback_type: "refocus" };
    app.do_disciple_action("feedback", refocus_content, this, this.model);
  }
});

app.AppView = Backbone.View.extend({
  // el - stands for element. Every view has a element associate in with HTML
  //      content will be rendered.
  el: '#discipleapp',
  // It's the first function called when this view it's instantiated.
  initialize: function(){
    this.input = this.$('#new-disciple');
    app.discipleList.on('add', this.addOne, this);
    app.discipleList.on('reset', this.addAll, this);
    app.discipleList.fetch();
    this.render();
  },
  events: {
    'keypress #new-disciple': 'createDiscipleOnEnter'
  },
  createDiscipleOnEnter: function(e){
    console.log(e.which);
    if ( e.which !== 13 || !this.input.val().trim() ) { // ENTER_KEY = 13
      return;
    }
    app.discipleList.create(this.newAttributes(), { wait: true });
    this.input.val(''); // clean input box
    app.log_append("Created new disciple!");
  },
  addOne: function(disciple){
    var view = new app.DiscipleView({model: disciple});
    $('#disciple-list').append(view.render().el);
  },
  addAll: function(){
    this.$('#disciple-list').html(''); // clean the disciple list
    app.discipleList.each(this.addOne, this);
  },
  newAttributes: function(){
    return {
      name: this.input.val().trim()
    }
  }
});

//--------------
// Routers
//--------------

//--------------
// Initializers
//--------------   

Backbone.history.start();    
app.appView = new app.AppView(); 

