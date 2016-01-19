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
package org.komodo.spi.udf;


/**
 *
 */
public class FunctionMethodDescriptor {

    private final Object metadataID;
    private final String name;
    private final String description;
    private final String category;
    private final String invocationClass;
    private final String invocationMethod;
    private final FunctionParameterDescriptor[] inputParameters;
    private final FunctionParameterDescriptor outputParameter;
    private final String schema;
    private String pushDownLiteral;
    private boolean deterministic;
    private boolean varArgs;

    /**
     * @param metadataID 
     * @param name
     * @param description
     * @param category
     * @param invocationClass
     * @param invocationMethod
     * @param inputParameters 
     * @param outputParameter 
     * @param schema 
     */
    public FunctionMethodDescriptor(Object metadataID, String name, String description, String category,
                                                          String invocationClass, String invocationMethod,
                                                          FunctionParameterDescriptor[] inputParameters,
                                                          FunctionParameterDescriptor outputParameter, String schema) {
        this.metadataID = metadataID;
        this.name = name;
        this.description = description;
        this.category = category;
        this.invocationClass = invocationClass;
        this.invocationMethod = invocationMethod;
        this.inputParameters = inputParameters;
        this.outputParameter = outputParameter;
        this.schema = schema;
    }

    /**
     * @return the metadataID
     */
    public Object getMetadataID() {
        return this.metadataID;
    }
    
    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return this.category;
    }

    /**
     * @return the invocationClass
     */
    public String getInvocationClass() {
        return this.invocationClass;
    }

    /**
     * @return the invocationMethod
     */
    public String getInvocationMethod() {
        return this.invocationMethod;
    }

    /**
     * @return the inputParameters
     */
    public FunctionParameterDescriptor[] getInputParameters() {
        return this.inputParameters;
    }

    /**
     * @return the outputParameter
     */
    public FunctionParameterDescriptor getOutputParameter() {
        return this.outputParameter;
    }
    
    /**
     * @return the schema
     */
    public String getSchema() {
        return this.schema;
    }

    /**
     * @return the pushDownLiteral
     */
    public String getPushDownLiteral() {
        return this.pushDownLiteral;
    }
    
    /**
     * @param literal
     */
    public void setPushDown(String literal) {
        pushDownLiteral = literal;
    }

    /**
     * @return the deterministic
     */
    public boolean isDeterministic() {
        return this.deterministic;
    }
    
    /**
     * @param deterministic
     */
    public void setDeterministic(boolean deterministic) {
        this.deterministic = deterministic;
    }
    
    /**
     * @return the varArgs
     */
    public boolean isVariableArgs() {
        return this.varArgs;
    }
    
    /**
     * @param varArgs
     */
    public void setVariableArgs(boolean varArgs) {
        this.varArgs = varArgs;
    }
}
