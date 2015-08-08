// setup
Elm.Native = Elm.Native || {};
Elm.Native.CustomGraphics = Elm.Native.CustomGraphics || {};

// definition
Elm.Native.CustomGraphics.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	if ('values' in Elm.Native.CustomGraphics) {
		return Elm.Native.CustomGraphics.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Signal = Elm.Native.Signal.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	var Element = Elm.Native.Graphics.Element.make(localRuntime);


	function renderDropDown(model)
	{
		var drop = Element.createNode('select');
		drop.style.border = '0 solid';
		drop.style.pointerEvents = 'auto';
		drop.style.display = 'block';

		drop.elm_values = List.toArray(model.values);
		drop.elm_handler = model.handler;
		var values = drop.elm_values;

		for (var i = 0; i < values.length; ++i)
		{
			var option = Element.createNode('option');
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
            if (name == model.selected) {option.selected = "true"} else { option.selected = null }
			drop.appendChild(option);
		}
		drop.addEventListener('change', function() {
			Signal.sendMessage(drop.elm_handler(drop.elm_values[drop.selectedIndex]._1));
		});

		return drop;
	}

	function updateDropDown(node, oldModel, newModel)
	{
		node.elm_values = List.toArray(newModel.values);
		node.elm_handler = newModel.handler;

		var values = node.elm_values;
		var kids = node.childNodes;
		var kidsLength = kids.length;

		var i = 0;
		for (; i < kidsLength && i < values.length; ++i)
		{
			var option = kids[i];
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
            if (name == newModel.selected) {option.selected = "true"} else { option.selected = null }
		}
		for (; i < kidsLength; ++i)
		{
			node.removeChild(node.lastChild);
		}
		for (; i < values.length; ++i)
		{
			var option = Element.createNode('option');
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
            if (name == newModel.selected) {option.selected = "true"} else { option.selected = null }
			node.appendChild(option);
		}
		return node;
	}

	function dropDown(handler, values, selected)
	{
		return A3(Element.newElement, 100, 24, {
			ctor: 'Custom',
			type: 'DropDown',
			render: renderDropDown,
			update: updateDropDown,
			model: {
				values: values,
                selected: selected,
				handler: handler
			}
		});
	}

	return Elm.Native.CustomGraphics.values = {
		dropDown: F3(dropDown),
	};

};
