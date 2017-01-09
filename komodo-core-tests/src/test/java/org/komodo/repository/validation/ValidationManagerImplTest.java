/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.repository.validation;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.io.File;
import java.util.List;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.ValidationManager;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ValidationManagerImplTest extends AbstractLocalRepositoryTest {

    private static String RULES_FILE_VERIFY_ERRORS = "verifyValidationErrors.xml";
    private static String RULES_FILE_VERIFY_UNIQUENESS_CHECKS = "verifyUniquenessChecks.xml";
    private static String RULES_FILE_ALL_CONSTRUCTS = "validationAllConstructs.xml";
    private static String RULES_FILE_RELATIONAL_RULES = "relationalValidationRulesDefault.xml";
    private static String RULES_FILE_NODE_NAME_RULE = "nodeNameRule.xml";
    private static String RULES_FILE_PROP_REQUIRED_RULE = "propRequiredRule.xml";
    private static String RULES_FILE_PROP_VALUE_RULE = "propValueRule.xml";
    private static String RULES_FILE_PROP_VALUE_RANGE_RULE = "propValueRangeRule.xml";
    private static String RULES_FILE_CHILD_REQUIRED_RULE = "childRequiredRule.xml";
    private static String RULES_FILE_CHILD_COUNT_RULE = "childCountRule.xml";
    private static String RULES_FILE_SNS_RULE = "sameNameSiblingRule.xml";
    private static String RULES_FILE_CHILD_TYPE_MUST_EXIST_RULE = "childTypeMustExistRule.xml";
    private static String RULES_FILE_CHILD_TYPE_MUST_NOT_EXIST_RULE = "childTypeMustNotExistRule.xml";
    private static String RULES_FILE_CHILD_PROP_MUST_EXIST_RULE = "childPropMustExistRule.xml";
    private static String RULES_FILE_CHILD_PROP_MUST_NOT_EXIST_RULE = "childPropMustNotExistRule.xml";

    private static String VDB_NAME = "vdbname";
    private static String VDB_TYPE = "vdb:virtualDatabase";
    private static String DATA_ROLE_NAME = "rolename";
    private static String DATA_ROLE_TYPE = "vdb:dataRole";
    private static String MODEL_TYPE = "vdb:declarativeModel";

    private static ValidationManager _validationMgr;

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        _validationMgr = _repo.getValidationManager();
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenNullRulesFile() throws Exception {
        _validationMgr.validateRules( null );
    }

    @Test
    public void shouldFindErrorsInXmlWithErrors() throws Exception {
        final int numErrors = 11;
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_VERIFY_ERRORS).getFile();
        final File testFile = new File(testFilePath);
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( numErrors ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Value 'BAD' is not facet-valid with respect to enumeration" ), is( true ) );
        assertThat( errors.get( 1 ).contains( "The value 'BAD' of attribute 'severity' on element 'nameValidation' is not valid" ),
                    is( true ) );
        assertThat( errors.get( 2 ).contains( "Value 'a' is not facet-valid with respect to pattern" ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Element 'minValue' must have no element [children], and the value must be valid" ),
                    is( true ) );
        assertThat( errors.get( 4 ).contains( "Invalid content was found starting with element 'minValue'" ), is( true ) );
        //assertThat( errors.get( 5 ).contains( "The content of element 'propertyValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 5 ).contains( "Invalid content was found starting with element 'valueValidation'" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "The content of element 'nodeValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 7 ).contains( "Invalid content was found starting with element 'childCountValidation'" ), is( true ) );
        assertThat( errors.get( 8 ).contains( "The content of element 'valueRangeValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 9 ).contains( "Value 'BAD_KEY' is not facet-valid with respect to enumeration" ), is( true ) );
        assertThat( errors.get( 10 ).contains( "The value 'BAD_KEY' of attribute 'key' on element 'message' is not valid with respect to its type, 'messageKeyType'" ),
                    is( true ) );
    }

    @Test
    public void shouldFindErrorsInXmlWithUniquenessProblems() throws Exception {
        final int numErrors = 16;
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_VERIFY_UNIQUENESS_CHECKS).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( numErrors ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Duplicate unique value [en]" ), is( true ) );
        assertThat( errors.get( 1 ).contains( "Duplicate unique value [es]" ), is( true ) );
        assertThat( errors.get( 2 ).contains( "Duplicate unique value [nz]" ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Duplicate unique value [vdb:connectionType]" ), is( true ) );
        assertThat( errors.get( 4 ).contains( "Duplicate unique value [vdb:dataRole] declared for identity constraint \"childValidationJcrNameKey\" of element \"nodeValidation\""), is( true ) );
        assertThat( errors.get( 5 ).contains( "Duplicate unique value [ca]" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "Duplicate unique value [ddl:statementOption]" ), is( true ) );
        // Duplicate nodeValidation types now allowed - added optional property Restrictions
        //assertThat( errors.get( 7 ).contains( "Duplicate unique value [vdb:virtualDatabase]" ), is( true ) );
        assertThat( errors.get( 7 ).contains( "Duplicate unique value [ddl:statementOption.value]" ), is( true ) );
        assertThat( errors.get( 8 ).contains( "Duplicate unique value [ab] declared for identity constraint \"propertyValueRuleDescriptionLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 9 ).contains( "Duplicate unique value [cd] declared for identity constraint \"propertyValueRuleDescriptionLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 10 ).contains( "Duplicate unique value [ef] declared for identity constraint \"propertyRelationshipRuleDescriptionLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 11 ).contains( "Duplicate unique value [gh] declared for identity constraint \"propertyValueRangeRuleDescriptionLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 12 ).contains( "Duplicate unique value [REQUIRED_PROPERTY_NOT_FOUND,ij] declared for identity constraint \"propertyValueRangeRuleMessageLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 13 ).contains( "Duplicate unique value [PATTERN_RULE_INVALID_PROPERTY_VALUE,kl] declared for identity constraint \"propertyValueRuleMessageLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 14 ).contains( "Duplicate unique value [PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND,mn] declared for identity constraint \"propertyRelationshipRuleMessageLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 15 ).contains( "Duplicate unique value [PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE,op] declared for identity constraint \"propertyValueRangeRuleMessageLocaleKey\" of element \"propertyValidation\"" ),
                    is( true ) );
    }

    @Test
    public void shouldParseXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_ALL_CONSTRUCTS).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.isEmpty(), is( true ) );
    }

    @Test
    public void shouldParseValidationRulesXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_RELATIONAL_RULES).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.isEmpty(), is( true ) );
    }

    @Test
    public void shouldVerifyImportedNodeNameRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_NODE_NAME_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedNodeNameRuleEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, "1"+VDB_NAME, VDB_TYPE );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_NODE_NAME_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB name does not match the specified pattern." ));
    }
    
    @Test
    public void shouldVerifyImportedNodeNameRuleEvaluationNotEnabled() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, "1"+VDB_NAME, VDB_TYPE );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_NODE_NAME_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];
        
        // disable the rule
        aRule.setEnabled(getTransaction(), false);
        
        // Evaluate kobject using the disabled rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getLevel(), is( Outcome.Level.INFO) );
        assertThat( result.getMessage(), is( "Rule \"vdb.name\" is disabled." ));
    }

    @Test
    public void shouldVerifyImportedPropRequiredRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.setProperty( getTransaction(), "vdb:connectionType", "propValue" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_PROP_REQUIRED_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedPropRequiredRuleEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_PROP_REQUIRED_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB 'vdb:connectionType' property must match the specified pattern." ));
    }

    @Test
    public void shouldVerifyImportedPropValueRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.setProperty( getTransaction(), "vdb:version", "2" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_PROP_VALUE_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedPropValueRuleEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.setProperty( getTransaction(), "vdb:version", "0" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_PROP_VALUE_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The value of property 'vdb:version' is invalid." ));
    }

    @Test
    public void shouldVerifyImportedPropValueRangeRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.setProperty( getTransaction(), "vdb:version", "2" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_PROP_VALUE_RANGE_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedPropValueRangeRuleEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.setProperty( getTransaction(), "vdb:version", "6" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_PROP_VALUE_RANGE_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB version must be between 1 and 5." ));
    }

    @Test
    public void shouldVerifyImportedChildRequiredEvaluationSuccess() throws Exception {
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        // Add five models
        for ( int i = 0; i < 5; ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), MODEL_TYPE );
        }

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_REQUIRED_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedChildRequiredEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_REQUIRED_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "A VDB must have between 1 and 5 child models." ));
    }

    @Test
    public void shouldVerifyImportedChildCountRuleEvaluationSuccess() throws Exception {
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        // Add five models
        for ( int i = 0; i < 5; ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), MODEL_TYPE );
        }

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_COUNT_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedChildCountRuleEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_COUNT_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "A VDB must have between 1 and 5 child models." ));
    }

    @Test
    public void shouldVerifyImportedSameNameSiblingRuleEvaluationSuccess() throws Exception {
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        // Add five models with different names
        for ( int i = 0; i < 5; ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), MODEL_TYPE );
        }

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_SNS_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedSameNameSiblingRuleEvaluationFailure() throws Exception {
        // Create a KomodoObject without the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        // Add 2 models with same name
        for ( int i = 0; i < 2; ++i ) {
            kobject.addChild( getTransaction(), ( "child" ), MODEL_TYPE );
        }
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_SNS_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load rules
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "A VDB model must have a unique name." ));
    }

    @Test
    public void shouldVerifyImportedRelationshipRuleChildTypesExistEvaluationSuccess() throws Exception {
        // create a VDB with one dataRole and one permission
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);
        roleObj.addChild( getTransaction(), "thePermission", "vdb:permission");

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_TYPE_MUST_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedRelationshipRuleChildTypesExistEvaluationFailure() throws Exception {
        // create a VDB with one dataRole and no permission
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_TYPE_MUST_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB dataRole must have a child permission." ));
    }

    @Test
    public void shouldVerifyImportedRelationshipRuleChildTypesAbsentEvaluationSuccess() throws Exception {
        // create a VDB with one dataRole and no permission
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_TYPE_MUST_NOT_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedRelationshipRuleChildTypesAbsentEvaluationFailure() throws Exception {
        // create a VDB with one dataRole and one permission
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        final KomodoObject roleObj = kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);
        roleObj.addChild( getTransaction(), "thePermission", "vdb:permission");

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_TYPE_MUST_NOT_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB dataRole must NOT have a child permission." ));
    }

    @Test
    public void shouldVerifyImportedRelationshipRulePropsExistEvaluationSuccess() throws Exception {
        // create a VDB with one dataRole and one permission
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);
        roleObj.setProperty( getTransaction(), "myProp", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_PROP_MUST_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedRelationshipRulePropsExistEvaluationFailure() throws Exception {
        // create a VDB with one dataRole with property that doesnt match
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);
        roleObj.setProperty( getTransaction(), "myProp2", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_PROP_MUST_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB dataRole must have a 'myProp' property" ));
    }

    @Test
    public void shouldVerifyImportedRelationshipRulePropsAbsentEvaluationSuccess() throws Exception {
        // create a VDB with one dataRole and one permission
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);
        roleObj.setProperty( getTransaction(), "myProp2", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_PROP_MUST_NOT_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( true ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
    }

    @Test
    public void shouldVerifyImportedRelationshipRulePropsAbsentEvaluationFailure() throws Exception {
        // create a VDB with one dataRole with 'myProp' property
        final KomodoObject kobject = _repo.add( getTransaction(), null, VDB_NAME, VDB_TYPE );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), DATA_ROLE_NAME, DATA_ROLE_TYPE);
        roleObj.setProperty( getTransaction(), "myProp", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource(RULES_FILE_CHILD_PROP_MUST_NOT_EXIST_RULE).getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );

        // Load the rule
        _validationMgr.importRules( getTransaction(), testFile, true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];

        // Evaluate the KomodoObject using the imported rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB dataRole must NOT have a 'myProp' property" ));
    }

}
