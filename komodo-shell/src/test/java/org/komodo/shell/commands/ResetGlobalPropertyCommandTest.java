/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell.commands;

import static org.junit.Assert.assertEquals;

import java.util.LinkedList;
import java.util.List;

import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Test Class to test {@link ResetGlobalPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class ResetGlobalPropertyCommandTest extends AbstractCommandTest {


	@Test( expected = AssertionError.class )
	public void shouldFailTooManyArgs( ) throws Exception {
	    final String[] commands = { "reset-global SHOW_HIDDEN_PROPERTIES false" }; // property value must not be specified
	    execute( commands );
	}


    @Test( expected = AssertionError.class )
    public void shouldFailBadGlobalProperty( ) throws Exception {
        final String[] commands = { "reset-global invalidProp" };
        execute( commands );
    }

    @Test
    public void shouldResetShowTypeInPrompt() throws Exception {
    	String propertyName=WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY;

    	wsStatus.setGlobalProperty(propertyName, negateBooleanValue(propertyName));
        final String[] commands = { "reset-global "+propertyName};
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // Check Context and property value
        assertEquals("/", wsStatus.getCurrentContextDisplayPath( null ));
        assertEquals(WorkspaceStatus.GLOBAL_PROPS.get(propertyName), wsStatus.getGlobalProperties(false).getProperty(propertyName));
    }

    @Test
    public void shouldResetAllGlobalProps() throws Exception {
    	String firstPropName=WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY;
    	String secondPropName=WorkspaceStatus.EXPORT_DEFAULT_DIR_KEY;
    	String secondPropNewVal="/tmp";
    	// Modify two global properties
    	wsStatus.setGlobalProperty(firstPropName, negateBooleanValue(firstPropName));
    	wsStatus.setGlobalProperty(secondPropName, secondPropNewVal);
    	// Issue command reset all
        final String[] commands = { "reset-global --all"};
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // Check Context and properties value
        assertEquals("/", wsStatus.getCurrentContextDisplayPath( null ));
        assertEquals(WorkspaceStatus.GLOBAL_PROPS.get(firstPropName), wsStatus.getGlobalProperties(false).getProperty(firstPropName));
        assertEquals(WorkspaceStatus.GLOBAL_PROPS.get(secondPropName), wsStatus.getGlobalProperties(false).getProperty(secondPropName));
    }

    @Test
    public void testTabCompleter()throws Exception{

        List<String> defaultValues = new LinkedList<>(WorkspaceStatus.GLOBAL_PROPS.keySet());
        defaultValues.add(0,"--all");
        assertTabCompletion("reset-global ", defaultValues);

        defaultValues.clear();
        String showTypeParam=WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY;
        defaultValues.add(showTypeParam);
        assertTabCompletion("reset-global "+showTypeParam.substring(0, showTypeParam.length()-3), defaultValues);

    }

    private String negateBooleanValue(String name){
    	boolean val=Boolean.parseBoolean(WorkspaceStatus.GLOBAL_PROPS.get(name));
    	return String.valueOf(!val).toUpperCase();
    }
}
