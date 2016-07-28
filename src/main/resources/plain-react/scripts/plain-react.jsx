var HelloDisplay = React.createClass({
  getInitialState: function() {
  	return {name: this.props.initialName};
  },
  handleNameChange: function(event) {
  	this.setState({name: event.target.value});
  },
  render: function() {
    return <div>
    	<p>Hello {this.state.name}</p>      
      <input type="text" name="name" value={this.state.name} onChange={this.handleNameChange} />
     </div>;
  }
});

ReactDOM.render(
  <HelloDisplay initialName="World!!" />,
  document.getElementById('content')
);
