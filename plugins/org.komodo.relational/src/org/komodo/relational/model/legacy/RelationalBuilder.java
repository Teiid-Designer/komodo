/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model.legacy;

import java.util.Map;

/**
 *
 */
public interface RelationalBuilder {
	
	/**
	 * Create a Relational object with specified type
	 * @param objectType the object type
	 * @return the relational object
	 */
	public RelationalObject create(int objectType);
	
	/**
	 * Create a Relational object with specified type and name
	 * @param objectType the object type
	 * @param name the object name
	 * @return the relational object
	 */
	public RelationalObject create(int objectType, String name);

	/**
	 * Create a Relational object with specified type, name and parent
	 * @param objectType the object type
	 * @param name the object name
	 * @param parent the objects parent
	 * @return the relational object
	 */
	public RelationalObject create(int objectType, String name, RelationalObject parent);
	
	/**
	 * Get the properties for the Relational Object
	 * @param object the relational object
	 * @return the name-value map of properties
	 */
	public Map<String,String> getProperties(RelationalObject object);
	
}
