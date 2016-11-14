
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

app.Adventure = Backbone.Model.extend({
  defaults: {
    name: "Go win",
    stages: []
  }
});

app.Stage = Backbone.Model.extend({
  defaults: {
    difficulty: 1,
    description: "cheer",
    failure: {
      message: "boo",
      consequences: [
        {
	  stat: "health",
	  change: -1
        }
      ]
    },
    success: {
      message: "yay",
      consequences: [
        {
	  stat: "health",
	  change: 1
        }
      ]
    }
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

app.StageList = Backbone.Collection.extend({
  model: app.Stage,
  url: '/1/stage',
  parse: function(response) {
    return response.stages;
  }
});

app.stageList = new app.StageList();

app.AdventureList = Backbone.Collection.extend({
  model: app.Adventure,
  url: '/1/adventure',
  parse: function(response) {
    return response.adventures;
  }
});

app.adventureList = new app.AdventureList();
//--------------
// Views
//--------------

app.AdventureView = Backbone.View.extend({
  tagName: 'li',
  template: _.template($('#adventure-template').html()),
  render: function(){
    var adventureStages = _.map(this.model.attributes.stages,
      function(stageId) {
        var stage = app.stageList.get(stageId);
	return {
	 id: stage.id,
	 description: stage.attributes.description,
	};
      });

    var allStages = app.stageList.map(function(stage) {
      return {
        id: stage.id,
        description: stage.attributes.description
      };
    });
    var templateParams = {
      adventure: this.model.toJSON(),
      adventureStages: adventureStages,
      allStages: allStages
    }
    var rendered = this.template(templateParams)
    this.$el.html(rendered);
    return this; // enable chained calls
  },
  doSave: function(e) {
    this.model.set({
     name: this.$('.adventure-name').val().trim(),
    });
    this.model.save()
  },
  events: {
    'click .update': 'doSave',
  }
});

app.StageView = Backbone.View.extend({
  tagName: 'li',
  template: _.template($('#stage-template').html()),
  render: function(){
    var rendered = this.template(this.model.toJSON())
    this.$el.html(rendered);
    return this; // enable chained calls
  },
  doSave: function(e) {
    this.model.set({
     description: this.$('.stage-description').val().trim(),
     difficulty: this.$('.difficulty').val()
    });
    this.model.save()
  },
  events: {
    'click .update': 'doSave',
  }
});

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
    app.discipleList.on('add', this.addOneDisciple, this);
    app.discipleList.on('reset', this.addAllDisciples, this);
    app.discipleList.fetch();
    app.stageList.on('add', this.addOneStage, this);
    app.stageList.on('reset', this.addAllStages, this);
    app.stageList.fetch();
    app.adventureList.on('add', this.addOneAdventure, this);
    app.adventureList.on('reset', this.addAllAdventures, this);
    app.adventureList.fetch();
    this.render();
  },
  events: {
    'keypress #new-disciple': 'createDiscipleOnEnter',
    'click #new-stage': 'createStage'
  },
  createDiscipleOnEnter: function(e){
    if ( e.which !== 13 || !this.input.val().trim() ) { // ENTER_KEY = 13
      return;
    }
    app.discipleList.create(this.newAttributes(), { wait: true });
    this.input.val(''); // clean input box
    app.log_append("Created new disciple!");
  },
  createStage: function(e) {
    app.stageList.create({}, { wait: true });
  },
  addOneDisciple: function(disciple){
    var view = new app.DiscipleView({model: disciple});
    $('#disciple-list').append(view.render().el);
  },
  addAllDisciples: function(){
    this.$('#disciple-list').html(''); // clean the disciple list
    app.discipleList.each(this.addOneDisciple, this);
  },
  addOneStage: function(stage){
    var view = new app.StageView({model: stage});
    $('#stage-list').append(view.render().el);
  },
  addAllStages: function(){
    this.$('#stage-list').html(''); // clean the disciple list
    app.stageList.each(this.addOneStage, this);
  },
  addOneAdventure: function(adventure){
    var view = new app.AdventureView({model: adventure});
    $('#adventure-list').append(view.render().el);
  },
  addAllAdventures: function(){
    this.$('#adventure-list').html(''); // clean the disciple list
    app.adventureList.each(this.addOneAdventure, this);
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

