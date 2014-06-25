package org.komodo.relational;


/**
 * Suite for all unit tests
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({ TestTable.class, TestColumn.class, TestProcedure.class, TestParameter.class,
	TestAccessPattern.class, TestForeignKey.class, TestModel.class, TestPrimaryKey.class, 
	TestProcedureResultSet.class, TestSchema.class, TestUniqueConstraint.class, TestView.class,})
public class AllTests {
    // nothing to do
}
