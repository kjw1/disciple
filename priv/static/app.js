
'use strict';

var app = {}; // create namespace for our app

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

