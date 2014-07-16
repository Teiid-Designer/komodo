/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.ddl.importer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Table;

/**
 *
 */
public class TestUtil {
	
    /**
     * Determine if the model has tables matching the list of supplied names
     * @param model the model
     * @param tableNames the list of names
     * @return 'true' if tables match, 'false' if not
     */
    public static boolean hasTables(Model model, List<String> tableNames) {
    	List<Table> tables = new ArrayList<Table>();
    	Collection<RelationalObject> children = model.getChildren();
    	for(RelationalObject relObj : children) {
    		if(relObj.getType()==RelationalConstants.TYPES.TABLE) {
    			tables.add((Table)relObj);
    		}
    	}
    	
    	// Must have equal numbers
    	if(tableNames.size()!=tables.size()) {
    		return false;
    	}
    	
    	for(Table table : tables) {
    		String tableNm = table.getName();
    		boolean found = false;
    		for(String tableName : tableNames) {
    			if(tableName.equalsIgnoreCase(tableNm)) {
    				found = true;
    				break;
    			}
    		}
    		if(!found) {
    			return false;
    		}
    	}
    	
    	return true;
    }

}
