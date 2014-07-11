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
package org.komodo.relational.core;

import java.util.Map;

import org.komodo.relational.constants.RelationalConstants.TYPES;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;


/**
 *
 */
public class RelationalBuilderImpl implements RelationalBuilder {
    
    private static RelationalBuilder builder;

    /**
     * Get the singleton instance of this builder
     * 
     * @return singleton {@link RelationalBuilderImpl}
     * 
     */
    public static RelationalBuilder getInstance() {
        if (builder == null) {
            builder = new RelationalBuilderImpl();
        }

        return builder;
    }

    private RelationalBuilderImpl() {
    }

	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalBuilder#create(int)
	 */
	@Override
	public RelationalObject create(int objectType) {
		RelationalObject obj = null;
		
		switch(objectType) {
		case TYPES.TABLE: 
			obj =  new Table();
			break;
		case TYPES.COLUMN:
			obj = new Column();
			break;
		case TYPES.PROCEDURE:
			obj = new Procedure();
			break;
		case TYPES.PARAMETER:
			obj = new Parameter();
			break;
		case TYPES.RESULT_SET:
			obj = new ProcedureResultSet();
			break;
		case TYPES.SCHEMA:
			obj = new Schema();
			break;
		case TYPES.VIEW:
			obj = new View();
			break;
		case TYPES.UC:
			obj = new UniqueConstraint();
			break;
		case TYPES.AP:
			obj = new AccessPattern();
			break;
		case TYPES.PK:
			obj = new PrimaryKey();
			break;
		case TYPES.FK:
			obj = new ForeignKey();
			break;
		case TYPES.INDEX:
			obj = new Index();
			break;
		case TYPES.MODEL:
			obj = new Model();
		}
		return obj;
	}

	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalBuilder#create(int, java.lang.String)
	 */
	@Override
	public RelationalObject create(int objectType, String name) {
		RelationalObject obj = create(objectType);
		obj.setName(name);
		return obj;
	}

	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalBuilder#create(int, java.lang.String, org.komodo.relational.model.RelationalObject)
	 */
	@Override
	public RelationalObject create(int objectType, String name,
			RelationalObject parent) {
		RelationalObject obj = create(objectType,name);
		obj.setParent(parent);
		return obj;
	}

	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalBuilder#getProperties(org.komodo.relational.model.RelationalObject)
	 */
	@Override
	public Map<String,String> getProperties(RelationalObject object) {
		return object.getProperties();
	}

}
