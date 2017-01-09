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
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Test Class to test setting WorkspaceStatus global and provider properties
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class WorkspaceStatusPropertyTest extends AbstractCommandTest {

    @Test
    public void shouldSetGlobalProperty() throws Exception {
        // Test default
        assertEquals( "true" , wsStatus.getGlobalProperties(false).getProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY));
        
        // Set a property
        wsStatus.setGlobalProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY, "false");
        
        // Test property changed
        assertEquals( "false" , wsStatus.getGlobalProperties(false).getProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY));
    }
    
    @Test
    public void shouldSetProvidedGlobalProperty() throws Exception {
        // Test no provided properties
        assertEquals( 0 , wsStatus.getProvidedGlobalProperties().size() );
        
        // Set a property
        wsStatus.setProvidedGlobalProperty("myPropKey", "aValue", "String");
        
        // Test property was set
        assertEquals( "aValue" , wsStatus.getProvidedGlobalProperties().getProperty("myPropKey") );
    }
    
    @Test
    public void shouldTestGlobalAfterAddingProvided() throws Exception {
        // Number of global at startup
        int startingGlobal = wsStatus.getGlobalProperties(false).size();
        
        // Test no provided properties
        assertEquals( 0 , wsStatus.getProvidedGlobalProperties().size() );
        
        // Set provided properties
        wsStatus.setProvidedGlobalProperty("myPropKey1", "aValue1", "String");
        wsStatus.setProvidedGlobalProperty("myPropKey2", "aValue2", "String");
        wsStatus.setProvidedGlobalProperty("myPropKey3", "aValue3", "String");
        
        // Count global after adding provided properties
        assertEquals( startingGlobal , wsStatus.getGlobalProperties(false).size() );
        // Count provided after adding provided properties
        assertEquals( 3 , wsStatus.getProvidedGlobalProperties().size() );
        
        // Ensure global does not contain provided properties
        assertEquals( "aValue1", wsStatus.getProvidedGlobalProperties().getProperty("myPropKey1"));
        assertEquals( "aValue2", wsStatus.getProvidedGlobalProperties().getProperty("myPropKey2"));
        assertEquals( "aValue3", wsStatus.getProvidedGlobalProperties().getProperty("myPropKey3"));
    }

    @Test
    public void shouldTestProvidedAfterSettingGlobal() throws Exception {
        // Number of provided at startup
        int startingProvided = wsStatus.getProvidedGlobalProperties().size();
        // Number of global at startup
        int startingGlobal = wsStatus.getGlobalProperties(false).size();
        
        // Set global properties
        wsStatus.setGlobalProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY, "false");
        wsStatus.setGlobalProperty(WorkspaceStatus.AUTO_COMMIT, "false");
        wsStatus.setGlobalProperty(WorkspaceStatus.EXPORT_DEFAULT_DIR_KEY, "exportDefaultDir");
        
        // Number of global doesnt change - they all have defaults
        assertEquals( startingGlobal , wsStatus.getGlobalProperties(false).size() );
        // Number of provided does not change
        assertEquals( startingProvided , wsStatus.getProvidedGlobalProperties().size() );
    }

}
