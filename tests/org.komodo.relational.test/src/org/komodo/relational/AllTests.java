package org.komodo.relational;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.relational.model.legacy.TestAccessPattern;
import org.komodo.relational.model.legacy.TestColumn;
import org.komodo.relational.model.legacy.TestForeignKey;
import org.komodo.relational.model.legacy.TestModel;
import org.komodo.relational.model.legacy.TestParameter;
import org.komodo.relational.model.legacy.TestPrimaryKey;
import org.komodo.relational.model.legacy.TestProcedure;
import org.komodo.relational.model.legacy.TestProcedureResultSet;
import org.komodo.relational.model.legacy.TestSchema;
import org.komodo.relational.model.legacy.TestTable;
import org.komodo.relational.model.legacy.TestUniqueConstraint;
import org.komodo.relational.model.legacy.TestView;


/**
 * Suite for all unit tests
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({ 
	TestTable.class, 
	TestColumn.class, 
	TestProcedure.class, 
	TestParameter.class,
	TestAccessPattern.class, 
	TestForeignKey.class, 
	TestModel.class, 
	TestPrimaryKey.class, 
	TestProcedureResultSet.class, 
	TestSchema.class, 
	TestUniqueConstraint.class, 
	TestView.class,})
public class AllTests {
    // nothing to do
}
