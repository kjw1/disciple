
'use strict';

var app = {}; // create namespace for our app

//--------------
// Models
//--------------

app.Disciple = Backbone.Model.extend({
  defaults: {
    name: "Bob"
  }
});
app.Todo = Backbone.Model.extend({
  defaults: {
    title: '',
    completed: false
  },
  toggle: function(){
    this.save({ completed: !this.get('completed')});
  }
});

//--------------
// Collections
//--------------
app.DiscipleList = Backbone.Collection.extend({
  model: app.Disciple,
  localStorage: new Store("disciple-local")
});

app.discipleList = new app.DiscipleList();

app.TodoList = Backbone.Collection.extend({
  model: app.Todo,
  localStorage: new Store("backbone-todo"),
  completed: function() {
    return this.filter(function( todo ) {
      return todo.get('completed');
    });
  },
  remaining: function() {
    return this.without.apply( this, this.completed() );
  }      
});

// instance of the Collection
app.todoList = new app.TodoList();

//--------------
// Views
//--------------

// renders individual disciple items list (li)
app.TodoView = Backbone.View.extend({
  tagName: 'li',
  template: _.template($('#item-template').html()),
  render: function(){
    this.$el.html(this.template(this.model.toJSON()));
    this.input = this.$('.edit');
    return this; // enable chained calls
  },
  initialize: function(){
    this.model.on('change', this.render, this);
    this.model.on('destroy', this.remove, this); // remove: Convenience Backbone's function for removing the view from the DOM.
  },      
  events: {
    'dblclick label' : 'edit',
    'keypress .edit' : 'updateOnEnter',
    'blur .edit' : 'close',
    'click .toggle': 'toggleCompleted',
    'click .destroy': 'destroy'
  },
  edit: function(){
    this.$el.addClass('editing');
    this.input.focus();
  },
  close: function(){
    var value = this.input.val().trim();
    if(value) {
      this.model.save({title: value});
    }
    this.$el.removeClass('editing');
  },
  updateOnEnter: function(e){
    if(e.which == 13){
      this.close();
    }
  },
  toggleCompleted: function(){
    this.model.toggle();
  },
  destroy: function(){
    this.model.destroy();
  }      
});

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
    app.discipleList.create(this.newAttributes());
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

app.Router = Backbone.Router.extend({
  routes: {
    '*filter' : 'setFilter'
  },
  setFilter: function(params) {
    console.log('app.router.params = ' + params);
    window.filter = params ? params.trim() || '' : '';
    app.todoList.trigger('reset');
  }
});     

//--------------
// Initializers
//--------------   

app.router = new app.Router();
Backbone.history.start();    
app.appView = new app.AppView(); 

