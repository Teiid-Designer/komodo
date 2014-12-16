/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.legacy;


/**
 * Factory for creating RelationalReference objects
 * This will create objects 'from scratch', or will create reference objects from EMF objects
 *
 * @since 8.2
 */
public class RelationalObjectFactory {

	@SuppressWarnings("javadoc")
	public static RelationalObjectFactory INSTANCE = new RelationalObjectFactory();
    
    /**
     * Create a  Model
     * @param modelName the name of the model
     * @return the new object
     */
    public Model createModel(String modelName) {
    	return new Model(modelName);
    }

    /**
     * Create a Schema
     * @return the new object
     */
    public Schema createSchema( ) {
    	return new Schema();
    }
    
    /**
     * Create a Schema
     * @param name the name of the model
     * @return the new object
     */
    public Schema createSchema(String name) {
    	return new Schema(name);
    }

    /**
     * Create a Table
     * @return the new object
     */
    public Table createTable( ) {
    	return new Table();
    }
    
    /**
     * Create a  Table
     * @param name the name of the object
     * @return the new object
     */
    public Table createTable(String name) {
    	return new Table(name);
    }

    /**
     * Create a View
     * @return the new object
     */
    public View createView( ) {
    	return new View();
    }

    /**
     * Create a View
     * @param name the name of the object
     * @return the new object
     */
    public View createView(String name) {
    	return new View(name);
    }
    
    /**
     * Create a Column
     * @return the new object
     */
    public Column createColumn( ) {
    	return new Column();
    }

    /**
     * Create a Column
     * @param name the name of the model
     * @return the new object
     */
    public Column createColumn(String name) {
    	return new Column(name);
    }

    /**
     * Create a AccessPattern
     * @return the new object
     */
    public AccessPattern createAccessPattern( ) {
    	return new AccessPattern();
    }
    
    /**
     * Create a AccessPattern
     * @param name the name of the model
     * @return the new object
     */
    public AccessPattern createAccessPattern(String name) {
    	return new AccessPattern(name);
    }
    
    /**
     * Create a ForeignKey
     * @return the new object
     */
    public ForeignKey createForeignKey( ) {
    	return new ForeignKey();
    }
    
    /**
     * Create a ForeignKey
     * @param name the name of the model
     * @return the new object
     */
    public ForeignKey createForeignKey(String name) {
    	return new ForeignKey(name);
    }
    
    /**
     * Create a Index
     * @return the new object
     */
    public Index createIndex( ) {
    	return new Index();
    }
    
    /**
     * Create a Index
     * @param name the name of the model
     * @return the new object
     */
    public Index createIndex(String name) {
    	return new Index(name);
    }
    
    /**
     * Create a Parameter
     * @return the new object
     */
    public Parameter createParameter( ) {
    	return new Parameter();
    }
    
    /**
     * Create a Parameter
     * @param name the name of the model
     * @return the new object
     */
    public Parameter createParameter(String name) {
    	return new Parameter(name);
    }
    
    /**
     * Create a  PrimaryKey
     * @return the new object
     */
    public PrimaryKey createPrimaryKey( ) {
    	return new PrimaryKey();
    }
    
    /**
     * Create a  PrimaryKey
     * @param name the name of the model
     * @return the new object
     */
    public PrimaryKey createPrimaryKey(String name) {
    	return new PrimaryKey(name);
    }
    
    /**
     * Create a Procedure
     * @return the new object
     */
    public Procedure createProcedure( ) {
    	return new Procedure();
    }
    
    /**
     * Create a Procedure
     * @param name the name of the model
     * @return the new object
     */
    public Procedure createProcedure(String name) {
    	return new Procedure(name);
    }
    
    /**
     * Create a ProcedureResultSet
     * @return the new object
     */
    public ProcedureResultSet createProcedureResultSet( ) {
    	return new ProcedureResultSet();
    }
    
    /**
     * Create a ProcedureResultSet
     * @param name the name of the model
     * @return the new object
     */
    public ProcedureResultSet createProcedureResultSet(String name) {
    	return new ProcedureResultSet(name);
    }
    
    /**
     * Create a UniqueConstraint
     * @return the new object
     */
    public UniqueConstraint createUniqueConstraint( ) {
    	return new UniqueConstraint();
    }
        
    /**
     * Create a UniqueConstraint
     * @param name the name of the model
     * @return the new object
     */
    public UniqueConstraint createUniqueConstraint(String name) {
    	return new UniqueConstraint(name);
    }
        
}
