/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.require("dojo.data.csv.*");
dojo.require("dojo.lang.type");

function run_all_tests() {
	test_data_csv_empty();
	test_data_csv_movies();
}

function test_data_csv_empty() {
	var csvStore = new dojo.data.csv.CsvStore({string: ""});
	
	// These are not items
	jum.assertTrue('100', !csvStore.isItem("foo"));
	jum.assertTrue('101', !csvStore.isItem());
	jum.assertTrue('102', !csvStore.isItem(csvStore));
	jum.assertTrue('103', !csvStore.isItem(103));
	
	// These are not identities
	jum.assertTrue('110', !csvStore.getByIdentity("foo"));
	jum.assertTrue('111', !csvStore.getByIdentity());
	jum.assertTrue('112', !csvStore.getByIdentity(csvStore));
	jum.assertTrue('113', !csvStore.getByIdentity(103));

	// find() returns a Result
	var result = csvStore.find() || null;
	jum.assertTrue('120', result != null);
	jum.assertTrue('121', result.getLength() == 0);
	jum.assertTrue('122', result.inProgress() == false);
	jum.assertTrue('123', result.getStore() == csvStore);
	
	var callback = function(item, index, result) { 
		// always fail if we get called
		jum.assertTrue('130', false);
	};
	result.forEach(callback);
}

function test_data_csv_movies() {
	var arrayOfRows = [];
	arrayOfRows.push('Title, Year, Producer');
	arrayOfRows.push('City of God, 2002, Katia Lund');
	arrayOfRows.push('Rain, 2001, Christine Jeffs');
	arrayOfRows.push('2001: A Space Odyssey, 1968, Stanley Kubrick');
	arrayOfRows.push('"This is a ""fake"" movie title", 1957, Sidney Lumet');
	arrayOfRows.push('Alien, 1979   , Ridley Scott');
	arrayOfRows.push('"The Sequel to ""Dances With Wolves.""", 1982, Ridley Scott');
	arrayOfRows.push('"Caine Mutiny, The", 1954, "Dymtryk ""the King"", Edward"');
	var csvString = arrayOfRows.join('\n');
	var numItems = arrayOfRows.length - 1;
	
	var csvStore = new dojo.data.csv.CsvStore({string: csvString});
	var result = csvStore.find() || null;
	jum.assertTrue('200', result != null);
	jum.assertTrue('201', result.getLength() == numItems);
	jum.assertTrue('202', result.inProgress() == false);
	jum.assertTrue('203', result.getStore() == csvStore);

	var callback = function(item, index, result) {
		jum.assertTrue('210', csvStore.isItem(item));
		
		var identity = csvStore.getIdentity(item);
		jum.assertTrue('211', identity != null);
		
		var itemToo = csvStore.getByIdentity(identity);
		jum.assertTrue('212', item === itemToo);
		
		var attributes = csvStore.getAttributes(item);
		for (var i in attributes) {
			var attribute = attributes[i];
			var value = csvStore.get(item, attribute);
			var values = csvStore.getValues(item, attribute);
			var valueToo = values[0];
			jum.assertTrue('213', value == valueToo);
			jum.assertTrue('214', csvStore.hasAttribute(item, attribute));
			jum.assertTrue('215', csvStore.hasAttributeValue(item, attribute, value));
			// dojo.debug(attribute + ': ' + value);
		}
		
		jum.assertTrue('216', result.inProgress());
	};
	result.forEach(callback);
	
	var onlyOnce = 0;
	var anotherCallback = function(item, index, result) {
		onlyOnce++;
		result.cancel();
	};
	result.forEach(anotherCallback);
	jum.assertTrue('220', onlyOnce == 1);
	
	var handlerObject = {};
	handlerObject.callbackMethod = function(item, index, result) {
		callback(item, index, result);
	}
	result.forEach(handlerObject.callbackMethod, handlerObject);
}


