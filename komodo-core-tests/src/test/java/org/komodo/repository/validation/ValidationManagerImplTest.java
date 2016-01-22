/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository.validation;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.io.File;
import java.util.List;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.ValidationManager;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ValidationManagerImplTest extends AbstractLocalRepositoryTest {

    private static ValidationManager _validationMgr;

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        _validationMgr = new ValidationManagerImpl( _repo );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenNullRulesFile() throws Exception {
        _validationMgr.validateRules( null );
    }

    @Test
    public void shouldFindErrorsInXmlWithErrors() throws Exception {
        final int numErrors = 12;
        String testFilePath = getClass().getClassLoader().getResource("verifyValidationErrors.xml").getFile();
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
        assertThat( errors.get( 5 ).contains( "The content of element 'propertyValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "Invalid content was found starting with element 'valueValidation'" ), is( true ) );
        assertThat( errors.get( 7 ).contains( "The content of element 'nodeValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 8 ).contains( "Invalid content was found starting with element 'requiredValidation'" ), is( true ) );
        assertThat( errors.get( 9 ).contains( "The content of element 'valueRangeValidation' is not complete" ), is( true ) );
        assertThat( errors.get( 10 ).contains( "Value 'BAD_KEY' is not facet-valid with respect to enumeration" ), is( true ) );
        assertThat( errors.get( 11 ).contains( "The value 'BAD_KEY' of attribute 'key' on element 'message' is not valid with respect to its type, 'messageKeyType'" ),
                    is( true ) );
    }

    @Test
    public void shouldFindErrorsInXmlWithUniquenessProblems() throws Exception {
        final int numErrors = 16;
        String testFilePath = getClass().getClassLoader().getResource("verifyUniquenessChecks.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( numErrors ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Duplicate unique value [en]" ), is( true ) );
        assertThat( errors.get( 1 ).contains( "Duplicate unique value [es]" ), is( true ) );
        assertThat( errors.get( 2 ).contains( "Duplicate unique value [nz]" ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Duplicate unique value [vdb:connectionType]" ), is( true ) );
        assertThat( errors.get( 4 ).contains( "Duplicate unique value [vdb:dataRole] declared for identity constraint of element \"nodeValidation\""), is( true ) );
        assertThat( errors.get( 5 ).contains( "Duplicate unique value [ca]" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "Duplicate unique value [ddl:statementOption]" ), is( true ) );
        // Duplicate nodeValidation types now allowed - added optional property Restrictions
        //assertThat( errors.get( 7 ).contains( "Duplicate unique value [vdb:virtualDatabase]" ), is( true ) );
        assertThat( errors.get( 7 ).contains( "Duplicate unique value [ddl:statementOption.value]" ), is( true ) );
        assertThat( errors.get( 8 ).contains( "Duplicate unique value [ab] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 9 ).contains( "Duplicate unique value [cd] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 10 ).contains( "Duplicate unique value [ef] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 11 ).contains( "Duplicate unique value [gh] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 12 ).contains( "Duplicate unique value [REQUIRED_PROPERTY_NOT_FOUND,ij] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 13 ).contains( "Duplicate unique value [PATTERN_RULE_INVALID_PROPERTY_VALUE,kl] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 14 ).contains( "Duplicate unique value [PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND,mn] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
        assertThat( errors.get( 15 ).contains( "Duplicate unique value [PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE,op] declared for identity constraint of element \"propertyValidation\"" ),
                    is( true ) );
    }

    @Test
    public void shouldParseXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("validationAllConstructs.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.isEmpty(), is( true ) );
    }

    @Test
    public void shouldParseValidationRulesXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("relationalValidationRulesDefault.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.isEmpty(), is( true ) );
    }
    
    @Test
    public void shouldVerifyImportedNodeNameRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, "vdbname", "vdb:virtualDatabase" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("nodeNameRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "1vdbname", "vdb:virtualDatabase" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource("nodeNameRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( testFile, getTransaction(), true );
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
    public void shouldVerifyImportedPropRequiredRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.setProperty( getTransaction(), "vdb:connectionType", "propValue" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("propRequiredRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource("propRequiredRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( testFile, getTransaction(), true );
        final Rule[] rules = _validationMgr.getAllRules(getTransaction());
        assertThat( rules.length, is( 1 ) );
        Rule aRule = rules[0];
        
        // Evaluate kobject using the loaded rule
        final Result result = aRule.evaluate( getTransaction(), kobject );
        assertThat( result.isOK(), is( false ) );
        assertThat( result.getPath(), is( kobject.getAbsolutePath() ) );
        assertThat( result.getRuleId(), is( aRule.getName( getTransaction() ) ) );
        assertThat( result.getMessage(), is( "The VDB connection type is required." ));
    }

    @Test
    public void shouldVerifyImportedPropValueRuleEvaluationSuccess() throws Exception {
        // Create a KomodoObject with the required property
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.setProperty( getTransaction(), "vdb:version", "2" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("propValueRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.setProperty( getTransaction(), "vdb:version", "0" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource("propValueRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.setProperty( getTransaction(), "vdb:version", "2" );
        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("propValueRangeRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.setProperty( getTransaction(), "vdb:version", "6" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource("propValueRangeRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( testFile, getTransaction(), true );
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
    public void shouldVerifyImportedChildCountRuleEvaluationSuccess() throws Exception {
        // setup KomodoObject
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        // Add five models
        for ( int i = 0; i < 5; ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), "vdb:declarativeModel" );
        }

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childCountRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource("childCountRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        // Add five models with different names
        for ( int i = 0; i < 5; ++i ) {
            kobject.addChild( getTransaction(), ( "child" + i ), "vdb:declarativeModel" );
        }

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("sameNameSiblingRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        // Add 2 models with same name
        for ( int i = 0; i < 2; ++i ) {
            kobject.addChild( getTransaction(), ( "child" ), "vdb:declarativeModel" );
        }
        commit();

        // Validate rules file
        String testFilePath = getClass().getClassLoader().getResource("sameNameSiblingRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load rules
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");
        roleObj.addChild( getTransaction(), "thePermission", "vdb:permission");

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childTypeMustExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childTypeMustExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childTypeMustNotExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        final KomodoObject roleObj = kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");
        roleObj.addChild( getTransaction(), "thePermission", "vdb:permission");

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childTypeMustNotExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");
        roleObj.setProperty( getTransaction(), "myProp", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childPropMustExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");
        roleObj.setProperty( getTransaction(), "myProp2", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childPropMustExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");
        roleObj.setProperty( getTransaction(), "myProp2", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childPropMustNotExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
        final KomodoObject kobject = _repo.add( getTransaction(), null, "kobject", "vdb:virtualDatabase" );
        final KomodoObject roleObj= kobject.addChild( getTransaction(), "theRole", "vdb:dataRole");
        roleObj.setProperty( getTransaction(), "myProp", "test" );

        commit();

        // Validate the rule file
        String testFilePath = getClass().getClassLoader().getResource("childPropMustNotExistRule.xml").getFile();
        final File testFile = new File( testFilePath );
        final List< String > errors = _validationMgr.validateRules( testFile );
        assertThat( errors.size(), is( 0 ) );
        
        // Load the rule
        _validationMgr.importRules( testFile, getTransaction(), true );
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
